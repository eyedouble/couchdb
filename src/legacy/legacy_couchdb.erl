%% @hidden
-module(legacy_couchdb).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-include("couchdb.hrl").

-define(TIMEOUT, infinity).

%% API urls
-export([
         get_uuid/1, get_uuids/2,        
        
         design_info/2, view_cleanup/1,
         stream_doc/1, end_doc_stream/1,
         copy_doc/2, copy_doc/3
         ]).

-opaque doc_stream() :: {atom(), any()}.
-export_type([doc_stream/0]).



%% --------------------------------------------------------------------
%% API functions.
%% --------------------------------------------------------------------




%% @doc Get one uuid from the server
%% @spec get_uuid(server()) -> lists()
get_uuid(Server) ->
    couchdb_uuids:get_uuids(Server, 1).

%% @doc Get a list of uuids from the server
%% @spec get_uuids(server(), integer()) -> lists()
get_uuids(Server, Count) ->
    couchdb_uuids:get_uuids(Server, Count).


design_info(#db{server=Server, name=DbName, options=Opts}, DesignName) ->
    Url = hackney_url:make_url(couchdb_httpc:server_url(Server),
                               [DbName, <<"_design">>, DesignName, <<"_info">>],
                               []),
    Resp = couchdb_httpc:db_request(get, Url, [], <<>>, Opts, [200]),
    case Resp of
        {ok, _, _, Ref} ->
            DesignInfo = couchdb_httpc:json_body(Ref),
            {ok, DesignInfo};
        Error ->
            Error
    end.

view_cleanup(#db{server=Server, name=DbName, options=Opts}) ->
    Url = hackney_url:make_url(couchdb_httpc:server_url(Server),
                               [DbName, <<"_view_cleanup">>],
                               []),
    Resp = couchdb_httpc:db_request(post, Url, [], <<>>, Opts, [200]),
    case Resp of
        {ok, _, _, Ref} ->
            catch hackney:skip_body(Ref),
            ok;
        Error ->
            Error
    end.






%
%   DROP THIS
%
% %% @doc Create a client for connecting to a database and create the
% %%      database if needed.
% %% @equiv open_or_create_db(Server, DbName, [], [])
% open_or_create_db(Server, DbName) ->
%     open_or_create_db(Server, DbName, [], []).

% %% @doc Create a client for connecting to a database and create the
% %%      database if needed.
% %% @equiv open_or_create_db(Server, DbName, Options, [])
% open_or_create_db(Server, DbName, Options) ->
%     open_or_create_db(Server, DbName, Options, []).

% %% @doc Create a client for connecting to a database and create the
% %%      database if needed.
% %% @spec open_or_create_db(server(), string(), list(), list()) -> {ok, db()|{error, Error}}
% open_or_create_db(#server{url=ServerUrl, options=Opts}=Server, DbName0,
%                   Options, Params) ->

%     DbName = couchdb_util:dbname(DbName0),
%     Url = hackney_url:make_url(ServerUrl, DbName, []),
%     Opts1 = couchdb_util:propmerge1(Options, Opts),
%     Resp = couchdb_httpc:request(get, Url, [], <<>>, Opts1),
%     case couchdb_httpc:db_resp(Resp, [200]) of
%         {ok, _Status, _Headers, Ref} ->
%             hackney:skip_body(Ref),
%             couchdb:database_record(Server, DbName, Options);
%         {error, not_found} ->
%             couchdb_databases:create(Server, DbName, Options, Params);
%         Error ->
%             Error
%     end.






%% @doc stream the multipart response of the doc API. Use this function
%% when you get `{ok, {multipart, State}}' from the function
%% `couchdb:open_doc/3'.
-spec stream_doc(doc_stream()) ->
    {doc, doc()}
    | {att, Name :: binary(), doc_stream()}
    | {att_body, Name :: binary(), Chunk :: binary(), doc_stream()}
    | {att_eof, Name :: binary(), doc_stream()}
    | eof
    | {error, term()}.
stream_doc({_Ref, Cont}) ->
    Cont().

%% @doc stop to receive the multipart response of the doc api and close
%% the connection.
-spec end_doc_stream(doc_stream()) -> ok.
end_doc_stream({Ref, _Cont}) ->
    hackney:close(Ref).






%% @doc duplicate a document using the doc API
copy_doc(#db{server=Server}=Db, Doc) ->
    [DocId] = get_uuid(Server),
    copy_doc(Db, Doc, DocId).

%% @doc copy a doc to a destination. If the destination exist it will
%% use the last revision, in other case a new doc is created with the
%% the current doc revision.
copy_doc(Db, Doc, Dest) when is_binary(Dest) ->
    Destination = case couchdb_documents:open(Db, Dest) of
        {ok, DestDoc} ->
            Rev = couchdb_doc:get_rev(DestDoc),
            {Dest, Rev};
        _ ->
            {Dest, <<>>}
    end,
    do_copy(Db, Doc, Destination);
copy_doc(Db, Doc, {Props}) ->
    DocId = proplists:get_value(<<"_id">>, Props),
    Rev = proplists:get_value(<<"_rev">>, Props, <<>>),
    do_copy(Db, Doc, {DocId, Rev}).

do_copy(Db, {Props}, Destination) ->
    case proplists:get_value(<<"_id">>, Props) of
        undefined ->
            {error, invalid_source};
        DocId ->
            DocRev = proplists:get_value(<<"_rev">>, Props, nil),
            do_copy(Db, {DocId, DocRev}, Destination)
    end;
do_copy(Db, DocId, Destination) when is_binary(DocId) ->
    do_copy(Db, {DocId, nil}, Destination);
do_copy(#db{server=Server, options=Opts}=Db, {DocId, DocRev},
        {DestId, DestRev}) ->

    Destination = case DestRev of
        <<>> -> DestId;
        _ -> << DestId/binary, "?rev=", DestRev/binary >>
    end,
    Headers = [{<<"Destination">>, Destination}],
    {Headers1, Params} = case {DocRev, DestRev} of
        {nil, _} ->
            {Headers, []};
        {_, <<>>} ->
            {[{<<"If-Match">>, DocRev} | Headers], []};
        {_, _} ->
            {Headers, [{<<"rev">>, DocRev}]}
    end,

    Url = hackney_url:make_url(couchdb_httpc:server_url(Server), couchdb_httpc:doc_url(Db, DocId),
                               Params),

    case couchdb_httpc:db_request(copy, Url, Headers1, <<>>,
                                    Opts, [201]) of
        {ok, _, _, Ref} ->
            {JsonProp} = couchdb_httpc:json_body(Ref),
            NewRev = couchdb_util:get_value(<<"rev">>, JsonProp),
            NewDocId = couchdb_util:get_value(<<"id">>, JsonProp),
            {ok, NewDocId, NewRev};
        Error ->
            Error
    end.











%% --------------------------------------------------------------------
%% private functions.
%% --------------------------------------------------------------------

%% add missing docid to a list of documents if needed
maybe_docid(Server, {DocProps}) ->
    case couchdb_util:get_value(<<"_id">>, DocProps) of
        undefined ->
            [DocId] = get_uuid(Server),
            {[{<<"_id">>, DocId}|DocProps]};
        _DocId ->
            {DocProps}
    end.
