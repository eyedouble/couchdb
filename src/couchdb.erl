%%% -*- erlang -*-
%%%
%%% This file is part of couchdb released under the MIT license.
%%% See the NOTICE for more information.

-module(couchdb).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-include("couchdb.hrl").

-define(TIMEOUT, infinity).

%% API urls
-export([server_connection/0, server_connection/1,
         server_connection/2, server_connection/4,
         get_uuid/1, get_uuids/2,        
         open_db/2, open_db/3,
         open_or_create_db/2, open_or_create_db/3, open_or_create_db/4,
         delete_db/1, delete_db/2,
         design_info/2, view_cleanup/1,
         stream_doc/1, end_doc_stream/1,
         delete_doc/2, delete_doc/3,
         save_docs/2, save_docs/3,
         delete_docs/2, delete_docs/3,
         copy_doc/2, copy_doc/3,
         lookup_doc_rev/2, lookup_doc_rev/3,
         fetch_attachment/3, fetch_attachment/4, stream_attachment/1,
         delete_attachment/3, delete_attachment/4,
         put_attachment/4, put_attachment/5, send_attachment/2,
         ensure_full_commit/1, ensure_full_commit/2,
         compact/1, compact/2,
         get_missing_revs/2]).

-opaque doc_stream() :: {atom(), any()}.
-export_type([doc_stream/0]).



%% --------------------------------------------------------------------
%% API functins.
%% --------------------------------------------------------------------

%% @doc Create a server for connectiong to a CouchDB node
%% @equiv server_connection("127.0.0.1", 5984, "", [], false)
server_connection() ->
    server_connection(<<"http://127.0.0.1:5984">>, []).


server_connection(URL) when is_list(URL) orelse is_binary(URL) ->
    server_connection(URL, []).



%% @doc Create a server for connectiong to a CouchDB node
%% @equiv server_connection(Host, Port, "", [])

server_connection(URL, Options) when is_list(Options) ->
    #server{url=hackney_url:fix_path(URL), options=Options};
server_connection(Host, Port) when is_integer(Port) ->
    server_connection(Host, Port, "", []).


%% @doc Create a server for connectiong to a CouchDB node
%%
%%      Connections are made to:
%%      ```http://Host:PortPrefix'''
%%
%%      If ssl is set https is used.
%%
%%      For a description of SSL Options, look in the <a href="http://www.erlang.org/doc/apps/ssl/index.html">ssl</a> manpage.
%%
-spec server_connection(Host::string(), Port::non_neg_integer(), Prefix::string(), OptionsList::list()) ->
        Server::server().
%% OptionsList() = [option()]
%% option() =
%%          {is_ssl, boolean()}                |
%%          {ssl_options, [SSLOpt]}            |
%%          {pool_name, atom()}                |
%%          {proxy_host, string()}             |
%%          {proxy_port, integer()}            |
%%          {proxy_user, string()}             |
%%          {proxy_password, string()}         |
%%          {basic_auth, {username(), password()}} |
%%          {cookie, string()}                 |
%%          {oauth, oauthOptions()}            |
%%          {proxyauth, [proxyauthOpt]}
%%
%% username() = string()
%% password() = string()
%% SSLOpt = term()
%% oauthOptions() = [oauth()]
%% oauth() =
%%          {consumer_key, string()} |
%%          {token, string()} |
%%          {token_secret, string()} |
%%          {consumer_secret, string()} |
%%          {signature_method, string()}
%%
%% proxyOpt = {X-Auth-CouchDB-UserName, username :: string()} |
%%            {X-Auth-CouchDB-Roles, roles :: string} | list_of_user_roles_separated_by_a_comma
%%            {X-Auth-CouchDB-Token: token :: string()} | authentication token. Optional, but strongly recommended to force token be required to prevent requests from untrusted sources.




server_connection(Host, Port, Prefix, Options)
        when is_integer(Port), Port =:= 443 ->
    BaseUrl = iolist_to_binary(["https://", Host, ":",
                                integer_to_list(Port)]),
    Url = hackney_url:make_url(BaseUrl, [Prefix], []),
    server_connection(Url, Options);
server_connection(Host, Port, Prefix, Options) ->
    Scheme = case proplists:get_value(is_ssl, Options) of
        true -> "https";
        _ -> "http"
    end,

    BaseUrl = iolist_to_binary([Scheme, "://", Host, ":",
                                integer_to_list(Port)]),
    Url = hackney_url:make_url(BaseUrl, [Prefix], []),
    server_connection(Url, Options).


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





%% @doc Create a client for connection to a database
%% @equiv open_db(Server, DbName, [])
open_db(Server, DbName) ->
    open_db(Server, DbName, []).

%% @doc Create a client for connection to a database
%% @spec open_db(Server::server(), DbName::string(), Options::optionList())
%%              -> {ok, db()}
open_db(#server{options=Opts}=Server, DbName, Options) ->
    Options1 = couchdb_util:propmerge1(Options, Opts),
    {ok, #db{server=Server, name=couchdb_util:dbname(DbName), options=Options1}}.


%% @doc Create a client for connecting to a database and create the
%%      database if needed.
%% @equiv open_or_create_db(Server, DbName, [], [])
open_or_create_db(Server, DbName) ->
    open_or_create_db(Server, DbName, [], []).

%% @doc Create a client for connecting to a database and create the
%%      database if needed.
%% @equiv open_or_create_db(Server, DbName, Options, [])
open_or_create_db(Server, DbName, Options) ->
    open_or_create_db(Server, DbName, Options, []).

%% @doc Create a client for connecting to a database and create the
%%      database if needed.
%% @spec open_or_create_db(server(), string(), list(), list()) -> {ok, db()|{error, Error}}
open_or_create_db(#server{url=ServerUrl, options=Opts}=Server, DbName0,
                  Options, Params) ->

    DbName = couchdb_util:dbname(DbName0),
    Url = hackney_url:make_url(ServerUrl, DbName, []),
    Opts1 = couchdb_util:propmerge1(Options, Opts),
    Resp = couchdb_httpc:request(get, Url, [], <<>>, Opts1),
    case couchdb_httpc:db_resp(Resp, [200]) of
        {ok, _Status, _Headers, Ref} ->
            hackney:skip_body(Ref),
            open_db(Server, DbName, Options);
        {error, not_found} ->
            couchdb_databases:create(Server, DbName, Options, Params);
        Error ->
            Error
    end.

%% @doc delete database
%% @equiv delete_db(Server, DbName)
delete_db(#db{server=Server, name=DbName}) ->
    delete_db(Server, DbName).

%% @doc delete database
%% @spec delete_db(server(), DbName) -> {ok, iolist()|{error, Error}}
delete_db(#server{url=ServerUrl, options=Opts}, DbName) ->
    Url = hackney_url:make_url(ServerUrl, couchdb_util:dbname(DbName), []),
    Resp = couchdb_httpc:request(delete, Url, [], <<>>, Opts),
    case couchdb_httpc:db_resp(Resp, [200]) of
        {ok, _, _, Ref} ->
            {ok, couchdb_httpc:json_body(Ref)};
        Error ->
            Error
    end.




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

%% @doc get the last revision of the document
lookup_doc_rev(Db, DocId) ->
    lookup_doc_rev(Db, DocId, []).

lookup_doc_rev(#db{server=Server, options=Opts}=Db, DocId, Params) ->
    DocId1 = couchdb_util:encode_docid(DocId),
    Url = hackney_url:make_url(couchdb_httpc:server_url(Server), couchdb_httpc:doc_url(Db, DocId1),
                               Params),
    case couchdb_httpc:db_request(head, Url, [], <<>>, Opts, [200]) of
        {ok, _, Headers} ->
            HeadersDict = hackney_headers:new(Headers),
            re:replace(hackney_headers:get_value(<<"etag">>, HeadersDict),
                <<"\"">>, <<>>, [global, {return, binary}]);
        Error ->
            Error
    end.

%% @doc fetch a document attachment
%% @equiv fetch_attachment(Db, DocId, Name, [])
fetch_attachment(Db, DocId, Name) ->
    fetch_attachment(Db, DocId, Name, []).

%% @doc fetch a document attachment
%% Options are
%% <ul>
%% <li>`stream': to start streaming an attachment. the function return
%% `{ok, Ref}' where is a ref to the attachment</li>
%% <li>Other options that can be sent using the REST API</li>
%% </ul>
%%
-spec fetch_attachment(db(), string(), string(),
                       list())
    -> {ok, binary()}| {ok, atom()} |{error, term()}.
fetch_attachment(#db{server=Server, options=Opts}=Db, DocId, Name, Options0) ->
    {Stream, Options} = case couchdb_util:get_value(stream, Options0) of
        undefined ->
            {false, Options0};
        true ->
            {true, proplists:delete(stream, Options0)};
        _ ->
            {false, proplists:delete(stream, Options0)}
    end,


    Options1 = couchdb_util:parse_options(Options),

    %% custom headers. Allows us to manage Range.
    {Options2, Headers} = case couchdb_util:get_value("headers", Options1) of
        undefined ->
            {Options1, []};
        Headers1 ->
            {proplists:delete("headers", Options1), Headers1}
    end,

    DocId1 = couchdb_util:encode_docid(DocId),
    Url = hackney_url:make_url(couchdb_httpc:server_url(Server),
                               [couchdb_httpc:db_url(Db), DocId1,
                                Name],
                               Options2),
    case hackney:get(Url, Headers, <<>>, Opts) of
        {ok, 200, _, Ref} when Stream /= true ->
            hackney:body(Ref);
        {ok, 200, _, Ref} ->
            {ok, Ref};
        {ok, 404, _, Ref} ->
            hackney:skip_body(Ref),
            {error, not_found};
        {ok, Status, Headers, Ref} ->
            {ok, Body} = hackney:body(Ref),
            {error, {bad_response, {Status, Headers, Body}}};

        Error ->
            Error
    end.

%% @doc fetch an attachment chunk.
%% Use this function when you pass the `stream' option to the
%% `couchdb:fetch_attachment/4' function.
%% This function return the following response:
%%      <dl>
%%          <dt>done</dt>
%%              <dd>You got all the attachment</dd>
%%          <dt>{ok, binary()}</dt>
%%              <dd>Part of the attachment</dd>
%%          <dt>{error, term()}</dt>
%%              <dd>n error occurred</dd>
%%      </dl>
%%

-spec stream_attachment(atom()) -> {ok, binary()}
    | done
    | {error, term()}.
stream_attachment(Ref) ->
    hackney:stream_body(Ref).

%% @doc put an attachment
%% @equiv put_attachment(Db, DocId, Name, Body, [])
put_attachment(Db, DocId, Name, Body)->
    put_attachment(Db, DocId, Name, Body, []).

%% @doc put an attachment
%% @spec put_attachment(Db::db(), DocId::string(), Name::string(),
%%                      Body::body(), Option::optionList()) -> {ok, iolist()}
%%       optionList() = [option()]
%%       option() = {rev, string()} |
%%                  {content_type, string()} |
%%                  {content_length, string()}
%%       body() = [] | string() | binary() | fun_arity_0() |
%%       {fun_arity_1(), initial_state(), stream}
%%       initial_state() = term()
put_attachment(#db{server=Server, options=Opts}=Db, DocId, Name, Body,
               Options) ->
    QueryArgs = case couchdb_util:get_value(rev, Options) of
        undefined -> [];
        Rev -> [{<<"rev">>, couchdb_util:to_binary(Rev)}]
    end,

    Headers = couchdb_util:get_value(headers, Options, []),


    FinalHeaders = lists:foldl(fun(Option, Acc) ->
                case Option of
                        {content_length, V} ->
                            V1 = couchdb_util:to_binary(V),
                            [{<<"Content-Length">>, V1}|Acc];
                        {content_type, V} ->
                            V1 = couchdb_util:to_binary(V),
                            [{<<"Content-Type">>, V1}|Acc];
                        _ ->
                            Acc
                end
        end, Headers, Options),

    DocId1 = couchdb_util:encode_docid(DocId),
    AttName = couchdb_util:encode_att_name(Name),
    Url = hackney_url:make_url(couchdb_httpc:server_url(Server), [couchdb_httpc:db_url(Db), DocId1,
                                                    AttName],
                               QueryArgs),

    case couchdb_httpc:db_request(put, Url, FinalHeaders, Body, Opts,
                                   [201]) of
        {ok, _, _, Ref} ->
            JsonBody = couchdb_httpc:json_body(Ref),
            {[{<<"ok">>, true}|R]} = JsonBody,
            {ok, {R}};
        {ok, Ref} ->
            {ok, Ref};
        Error ->
            Error
    end.

%% @doc send an attachment chunk
%% Msg could be Data, eof to stop sending.
send_attachment(Ref, eof) ->
    case hackney:finish_send_body(Ref) of
        ok ->
            Resp =  hackney:start_response(Ref),
            couchdb_httpc:reply_att(Resp);
        Error ->
            Error
    end;
send_attachment(Ref, Msg) ->
    Reply = hackney:send_body(Ref, Msg),
    couchdb_httpc:reply_att(Reply).


%% @doc delete a document attachment
%% @equiv delete_attachment(Db, Doc, Name, [])
delete_attachment(Db, Doc, Name) ->
    delete_attachment(Db, Doc, Name, []).

%% @doc delete a document attachment
%% @spec(db(), string()|list(), string(), list() -> {ok, Result} | {error, Error}
delete_attachment(#db{server=Server, options=Opts}=Db, DocOrDocId, Name,
                  Options) ->
    Options1 = couchdb_util:parse_options(Options),
    {Rev, DocId} = case DocOrDocId of
        {Props} ->
            Rev1 = couchdb_util:get_value(<<"_rev">>, Props),
            DocId1 = couchdb_util:get_value(<<"_id">>, Props),
            {Rev1, DocId1};
        DocId1 ->
            Rev1 = couchdb_util:get_value("rev", Options1),
            {Rev1, DocId1}
    end,
    case Rev of
        undefined ->
           {error, rev_undefined};
        _ ->
            Options2 = case couchdb_util:get_value("rev", Options1) of
                undefined ->
                    [{<<"rev">>, couchdb_util:to_binary(Rev)}|Options1];
                _ ->
                    Options1
            end,
            Url = hackney_url:make_url(couchdb_httpc:server_url(Server), [couchdb_httpc:db_url(Db),
                                                            DocId,
                                                            Name],
                                       Options2),

            case couchdb_httpc:db_request(delete, Url, [], <<>>, Opts,
                                            [200]) of
            {ok, _, _, Ref} ->
                {[{<<"ok">>,true}|R]} = couchdb_httpc:json_body(Ref),
                {ok, {R}};
            Error ->
                Error
            end
    end.

%% @doc commit all docs in memory
%% @equiv ensure_full_commit(Db, [])
ensure_full_commit(Db) ->
    ensure_full_commit(Db, []).

%% @doc commit all docs in memory
-spec ensure_full_commit(Db::db(), Options::list())
    -> {ok, InstancestartTime :: binary()}
    | {error, term()}.
ensure_full_commit(#db{server=Server, options=Opts}=Db, Options) ->
    Url = hackney_url:make_url(couchdb_httpc:server_url(Server), [couchdb_httpc:db_url(Db),
                                                    <<"_ensure_full_commit">>],
                               Options),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    case couchdb_httpc:db_request(post, Url, Headers, <<>>, Opts, [201]) of
        {ok, _, _, Ref} ->
            {Props} = couchdb_httpc:json_body(Ref),
            {ok, proplists:get_value(<<"instance_start_time">>, Props)};
        Error ->
            Error
    end.

%% @doc Compaction compresses the database file by removing unused
%% sections created during updates.
%% See [http://wiki.apache.org/couchdb/Compaction] for more informations
%% @spec compact(Db::db()) -> ok|{error, term()}
compact(#db{server=Server, options=Opts}=Db) ->
    Url = hackney_url:make_url(couchdb_httpc:server_url(Server), [couchdb_httpc:db_url(Db),
                                                    <<"_compact">>],
                               []),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    case couchdb_httpc:db_request(post, Url, Headers, <<>>, Opts, [202]) of
        {ok, _, _, Ref} ->
            hackney:skip_body(Ref),
            ok;
        Error ->
            Error
    end.
%% @doc Like compact/1 but this compacts the view index from the
%% current version of the design document.
%% See [http://wiki.apache.org/couchdb/Compaction#View_compaction] for more informations
%% @spec compact(Db::db(), ViewName::string()) -> ok|{error, term()}
compact(#db{server=Server, options=Opts}=Db, DesignName) ->
    Url = hackney_url:make_url(couchdb_httpc:server_url(Server), [couchdb_httpc:db_url(Db),
                                                   <<"_compact">>,
                                                   DesignName], []),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    case couchdb_httpc:db_request(post, Url, Headers, <<>>, Opts, [202]) of
        {ok, _, _, Ref} ->
            hackney:skip_body(Ref),
            ok;
        Error ->
            Error
    end.


%% @doc get missing revisions
-spec get_missing_revs(#db{}, [{binary(), [binary()]}]) ->
    {ok, [{DocId :: binary(), [MissingRev :: binary()], [
                    PossibleAncestor :: binary()]}]}
    | {error, term()}.
get_missing_revs(#db{server=Server, options=Opts}=Db, IdRevs) ->
    Json = couchdb_ejson:encode({IdRevs}),
    Url = hackney_url:make_url(couchdb_httpc:server_url(Server), [couchdb_httpc:db_url(Db),
                                                   <<"_revs_diff">>],
                              []),

    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    case couchdb_httpc:db_request(post, Url, Headers, Json, Opts,
                                    [200]) of
        {ok, _, _, Ref} ->
            {Props} = couchdb_httpc:json_body(Ref),
            Res = lists:map(fun({Id, {Result}}) ->
                            MissingRevs = proplists:get_value(
                                    <<"missing">>, Result
                            ),
                            PossibleAncestors = proplists:get_value(
                                <<"possible_ancestors">>, Result, []
                            ),
                            {Id, MissingRevs, PossibleAncestors}
                    end, Props),
            {ok, Res};
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
