-module(couchdb_documents).

-include("couchdb.hrl").
-include("../../dev.hrl").

-export([
    exists/2
    ,lookup_rev/2
    ,lookup_rev/3
    ,get/2
    ,get/3
    ,save/2
    ,save/3
    ,save/4
    ,delete/2
    ,delete/3
]).


%% @reference CouchDB Docs 1.4.1/HEAD
%% @doc test if doc with uuid exists in the given db
%% @spec doc_exists(db(), string()) -> boolean()
exists(#db{server=Server, options=Opts}=Db, DocId) ->
    DocId1 = couchdb_util:encode_docid(DocId),
    Url = hackney_url:make_url(couchdb_httpc:server_url(Server), couchdb_httpc:doc_url(Db, DocId1), []),
    case couchdb_httpc:db_request(head, Url, [], <<>>, Opts, [200]) of
        {ok, _, _} -> true;
        _Error -> false
    end.

%% @reference CouchDB Docs 1.4.1/HEAD
%% @doc get the last revision of the document
lookup_rev(Db, DocId) ->
    lookup_rev(Db, DocId, []).

lookup_rev(#db{server=Server, options=Opts}=Db, DocId, Params) ->
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

%% @reference CouchDB Docs 1.4.1/GET
%% @doc open a document
%% @equiv open_doc(Db, DocId, [])
get(Db, DocId) ->
    get(Db, DocId, []).

%% @reference CouchDB Docs 1.4.1/GET
%% @doc open a document
%% Params is a list of query argument. Have a look in CouchDb API
-spec(get(Db::db(), DocId::binary(), Params::list()) -> {ok, map()} | {error, term()}).
get(#db{server=Server, options=Opts}=Db, DocId, Params) ->
    DocId1 = couchdb_util:encode_docid(DocId),

    %% is there any accepted content-type passed to the params?
    {Accept, Params1} = case proplists:get_value(accept, Params) of
        unefined -> {any, Params};
        A -> {A, proplists:delete(accept, Params)}
    end,
    %% set the headers with the accepted content-type if needed
    Headers = case {Accept, proplists:get_value("attachments", Params)} of
        {any, true} ->
            %% only use the more efficient method when we get the
            %% attachments so we don't use much bandwidth.
            [{<<"Accept">>, <<"multipart/related">>}];
        {Accept, _} when is_binary(Accept) ->
            %% accepted content-type has been forced
            [{<<"Accept">>, Accept}];
        _ ->
            []
    end,
    Url = hackney_url:make_url(couchdb_httpc:server_url(Server), couchdb_httpc:doc_url(Db, DocId1),
                               Params1),
    case couchdb_httpc:db_request(get, Url, Headers, <<>>, Opts,
                                    [200, 201]) of
        {ok, _, RespHeaders, Ref} ->
            case hackney_headers:parse(<<"content-type">>, RespHeaders) of
                {<<"multipart">>, _, _} ->
                    %% we get a multipart request, start to parse it.
                    InitialState =  {Ref, fun() ->
                                    couchdb_httpc:wait_mp_doc(Ref, <<>>)
                            end},
                    {ok, {multipart, InitialState}};
                _ ->
                    {ok, couchdb_httpc:json_body(Ref)}
            end;        
        Error ->
            Error
    end.

%% @reference CouchDB Docs 1.4.1/PUT
%% @doc save a document
%% @equiv save(Db, Doc, [])
save(Db, Doc) ->
    save(Db, Doc, []).

%% @doc save a *document
%% A document is a Json object like this one:
%%
%%      ```{[
%%          {<<"_id">>, <<"myid">>},
%%          {<<"title">>, <<"test">>}
%%      ]}'''
%%
%% Options are arguments passed to the request. This function return a
%% new document with last revision and a docid. If _id isn't specified in
%% document it will be created. Id is created by extracting an uuid from
%% the couchdb node.
%%
%% @spec save(Db::db(), Doc, Options::list()) -> {ok, Doc1}|{error, Error}
save(Db, Doc, Options) ->
    save(Db, Doc, [], Options).


%% @doc save a *document with all its attacjments
%% A document is a Json object like this one:
%%
%%      ```{[
%%          {<<"_id">>, <<"myid">>},
%%          {<<"title">>, <<"test">>}
%%      ]}'''
%%
%% Options are arguments passed to the request. This function return a
%% new document with last revision and a docid. If _id isn't specified in
%% document it will be created. Id is created by extracting an uuid from
%% the couchdb node.
%%
%% If the attachments is not empty, the doc will be sent as multipart.
%% Attachments are passed as a list of the following tuples:
%%
%% - `{Name :: binary(), Bin :: binary()}'
%% - `{Name :: binary(), Bin :: binary(), Encoding :: binary()}'
%% - `{ Name :: binary(), Bin :: binary(), Type :: binary(), Encoding :: binary()}'
%% - `{ Name :: binary(), {file, Path ::  string()}}'
%% - `{ Name :: binary(), {file, Path ::  string()}, Encoding :: binary()}'
%% - `{ Name :: binary(), Fun :: fun(), Length :: integer()}'
%% - `{ Name :: binary(), Fun :: fun(), Length :: integer(), Encoding :: binary()}'
%% - `{Name :: binary(), Fun :: fun(), Length :: integer(), Type :: binary(), Encoding :: binary()}'
%% - `{ Name :: binary(), {Fun :: fun(), Acc :: any()}, Length :: integer()}'
%% - `{ Name :: binary(), {Fun :: fun(), Acc :: any()}, Length :: integer(), Encoding :: binary()}'
%% - `{ Name :: binary(), {Fun :: fun(), Acc :: any()}, Length :: integer(), Type :: binary(), Encoding :: binary()}.'
%%
%% where `Type` is the content-type of the attachments (detected in other
%% case) and `Encoding` the encoding of the attachments:
%% `<<"identity">>' if normal or `<<"gzip">>' if the attachments is
%% gzipped.

-spec save(Db::db(), doc(), mp_attachments(), Options::list()) -> {ok, doc()} | {error, term()}.
save(#db{server=Server, options=_Opts}=Db, #{}=Doc, Atts, Options) ->
    % DocId = case couchdb_util:get_value(<<"_id">>, Props) of
    %     undefined ->
    %         [Id] = get_uuid(Server),
    %         Id;
    %     DocId1 ->
    %         couchdb_util:encode_docid(DocId1)
    % end,

    % PREPARE DOC ID 
    DocId = case maps:get(<<"_id">>, Doc, nil) of        
        Id when is_binary(Id) -> couchdb_util:encode_docid(Id);        
        nil ->
            quickrand:seed ( ),
            list_to_binary ( uuid:uuid_to_string ( uuid:get_v4_urandom ( ) ) )
    end,
    
    Url = hackney_url:make_url(couchdb_httpc:server_url(Server), couchdb_httpc:doc_url(Db, DocId), Options),

    % case Atts of
    %     [] ->
    %         JsonDoc = couchdb_ejson:encode(Doc),
    %         Headers = [{<<"Content-Type">>, <<"application/json">>}],
    %         case couchdb_httpc:db_request(put, Url, Headers, JsonDoc, Opts,
    %                                 [200, 201, 202]) of
    %             {ok, _, _, Ref} ->
    %                 {JsonProp} = couchdb_httpc:json_body(Ref),
    %                 NewRev = couchdb_util:get_value(<<"rev">>, JsonProp),
    %                 NewDocId = couchdb_util:get_value(<<"id">>, JsonProp),
    %                 Doc1 = couchdb_doc:set_value(<<"_rev">>, NewRev,
    %                     couchdb_doc:set_value(<<"_id">>, NewDocId, Doc)),
    %                 {ok, Doc1};
    %             Error ->
    %                 Error
    %         end;
    %     _ ->
    %         Boundary = couchdb_uuids:random(),

    %         %% for now couchdb can't received chunked multipart stream
    %         %% so we have to calculate the content-length. It also means
    %         %% that we need to know the size of each attachments. (Which
    %         %% should be expected).
    %         {CLen, JsonDoc, Doc2} = couchdb_httpc:len_doc_to_mp_stream(Atts, Boundary, Doc),
    %         CType = <<"multipart/related; boundary=\"",
    %                   Boundary/binary, "\"" >>,

    %         Headers = [{<<"Content-Type">>, CType},
    %                    {<<"Content-Length">>, hackney_bstr:to_binary(CLen)}],

    %         case couchdb_httpc:request(put, Url, Headers, stream,
    %                                      Opts) of
    %             {ok, Ref} ->
    %                 couchdb_httpc:send_mp_doc(Atts, Ref, Boundary, JsonDoc, Doc2);
    %             Error ->
    %                 Error
    %         end
    % end

    % Send off single-part if no attachments or multipart if attachments.
    case Atts of
        [] -> send_document_singlepart(Db, Url, Doc);
        _ -> send_document_multipart(Db, Url, Doc, Atts)
    end.

%% @reference CouchDB Docs 1.4.1/DELETE
%% @doc Deletes a list of documents
%% if you want to make sure the doc it emptied on delete, use the option
%% {empty_on_delete,  true} or pass a doc with just _id and _rev
%% members.
delete(#db{}=Db, #{<<"_id">> := <<_Id/binary>>, <<"_rev">> := <<_Rev/binary>>}=Document) ->
    ?PRINT(Document),
    delete(Db, [Document], []);
delete(#db{}=Db, [#{<<"_id">> := <<_Id/binary>>, <<"_rev">> := <<_Rev/binary>>}=_Car | _Cdr]=Documents) ->
    delete(Db, Documents, []).

%% @doc Deletes a list of documents
%% if you want to make sure the doc it emptied on delete, use the option
%% {empty_on_delete,  true} or pass a doc with just _id and _rev
%% members.
-spec(delete(Db::db(), map() | list(), Options::list()) -> {ok, _Result}|{error, _Error}).
delete(#db{}=Db, #{<<"_id">> := <<_Id/binary>>, <<"_rev">> := <<_Rev/binary>>}=Document, Options) when is_list(Options) ->
    delete(Db, [Document], Options);
delete(#db{}=Db, [#{<<"_id">> := <<_Id/binary>>, <<"_rev">> := <<_Rev/binary>>}=_Car | _Cdr]=Documents, Options) when is_list(Options) ->
    Empty =[true || {empty_on_delete, true} <- Options] =:= [true],

    FinalDocs = case Empty of
        true ->
            [ #{
                <<"_id">> => Id, 
                <<"_rev">> => Rev,
                <<"_deleted">> => true             
            } || #{<<"_id">> := <<Id/binary>>, <<"_rev">> := <<Rev/binary>>}=_Doc <- Documents];
        false -> [ maps:put(<<"_deleted">>, true, Doc) || #{<<"_id">> := <<_Id1/binary>>, <<"_rev">> := <<_Rev1/binary>>}=Doc <- Documents]
    end,
    couchdb_databases:bulk_docs_save(Db, FinalDocs, []).
    




%% @reference CouchDB Docs 1.4.1.7/COPY
%% @doc Copying to an Existing Document
%%
%%
%% Not Implemented Yet. Old Implementation looked quite bad, dropped in favour of other
%% functionality first.



%
%   PRIVATE
%
%% @private
send_document_singlepart(#db{server=_Server, options=Opts}=_Db, Url, #{}=Doc) ->
    JsonDoc = couchdb_ejson:encode(Doc),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    case couchdb_httpc:db_request(put, Url, Headers, JsonDoc, Opts, [200, 201, 202]) of
        {ok, _Res_code, _Headers, Ref} ->
            case couchdb_httpc:json_body(Ref) of
                #{<<"id">> := Id, <<"rev">> := Rev, <<"ok">> := true}=_Res1 ->
                    {ok, maps:merge(Doc, #{<<"_id">> => Id, <<"_rev">> => Rev})};
                Err0 -> Err0
            end;
        Error -> Error
    end.

%% @private
send_document_multipart(#db{server=_Server, options=Opts}=_Db, Url, #{}=Doc, Atts) -> 
   Boundary = couchdb_uuids:random(),

    %% for now couchdb can't received chunked multipart stream
    %% so we have to calculate the content-length. It also means
    %% that we need to know the size of each attachments. (Which
    %% should be expected).
    {CLen, JsonDoc, Doc2} = couchdb_httpc:len_doc_to_mp_stream(Atts, Boundary, Doc),
    CType = <<"multipart/related; boundary=\"", Boundary/binary, "\"" >>,

    Headers = [{<<"Content-Type">>, CType},
                {<<"Content-Length">>, hackney_bstr:to_binary(CLen)}],

    case couchdb_httpc:request(put, Url, Headers, stream, Opts) of
        {ok, Ref} ->
            couchdb_httpc:send_mp_doc(Atts, Ref, Boundary, JsonDoc, Doc2);
        Error ->
            Error
    end.