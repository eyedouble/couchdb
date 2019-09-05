-module(couchdb_databases).

-include("couchdb.hrl").
-include("../dev.hrl").

-export([
    exists/2
    ,info/1
    ,create/2
    ,create/3
    ,create/4
    ,delete/1
    ,delete/2
    ,bulk_docs_save/2
    ,bulk_docs_save/3
    ,ensure_full_commit/1
    ,ensure_full_commit/2
    ,get_missing_revs/2
]).

%% @reference CouchDB Docs 1.3.1/HEAD
%% @doc Returns the HTTP Headers containing a minimal amount of information about the specified 
%% database. Since the response body is empty, using the HEAD method is a lightweight way to 
%% check if the database exists already or not.
%% @spec exists(server(), binary()) -> boolean()
exists(#server{url=ServerUrl, options=Opts}, DbName) ->
    Url = hackney_url:make_url(ServerUrl, couchdb_util:dbname(DbName), []),
    case couchdb_httpc:db_request(head, Url, [], <<>>, Opts, [200]) of
        {ok, 200, _}->
            true;
        _Error ->
            false
    end.

%% @reference CouchDB Docs 1.3.1/GET
%% @doc get database info
%% @spec info(db()) -> {ok, iolist()|{error, Error}}
info(#db{server=Server, name=DbName, options=Opts}) ->
    Url = hackney_url:make_url(couchdb_httpc:server_url(Server), couchdb_util:dbname(DbName), []),
    case couchdb_httpc:db_request(get, Url, [], <<>>, Opts, [200]) of
        {ok, _Status, _Headers, Ref} ->
            Infos = couchdb_httpc:json_body(Ref),
            {ok, Infos};
        {error, not_found} ->
            {error, db_not_found};
       Error ->
          Error
    end.

%% @reference CouchDB Docs 1.3.1/PUT
%% @doc Create a database and a client for connectiong to it.
%% @equiv create(Server, DbName, [], [])
create(Server, DbName) ->
    create(Server, DbName, [], []).

%% @doc Create a database and a client for connectiong to it.
%% @equiv create(Server, DbName, Options, [])
create(Server, DbName, Options) ->
    create(Server, DbName, Options, []).

%% DB names must conform to ^[a-z][a-z0-9_$()+/-]*$
%% @doc Create a database and a client for connectiong to it.
%%
%%      Connections are made to:
%%      ```http://Host:PortPrefix/DbName'''
%%
%% If ssl is set https is used. See server_connections for options.
%% Params is a list of optionnal query argument you want to pass to the
%% db. Useful for bigcouch for example.
%%
%% @spec create(Server::server(), DbName::string(),
%%                 Options::optionList(), Params::list()) -> {ok, db()|{error, Error}}
create(#server{url=ServerUrl, options=Opts}=Server, DbName0, Options, Params) ->
    DbName = couchdb_util:dbname(DbName0),
    Options1 = couchdb_util:propmerge1(Options, Opts),
    Url = hackney_url:make_url(ServerUrl, DbName, Params),
    Resp = couchdb_httpc:db_request(put, Url, [], <<>>, Options1, [201]),
    case Resp of
        {ok, _Status, _Headers, Ref} ->
            hackney:skip_body(Ref),
            {ok, #db{server=Server, name=DbName, options=Options1}};
        {error, precondition_failed} ->
            {error, db_exists};
       Error ->
          Error
    end.


%% @reference CouchDB Docs 1.3.1/DELETE
%% @doc delete database
%% @equiv delete(Server, DbName)
delete(#db{server=Server, name=DbName}) ->
    delete(Server, DbName).

%% @doc delete database
%% @spec delete(server(), DbName) -> {ok, iolist()|{error, Error}}
delete(#server{url=_ServerUrl, options=_Opts}=Server, #db{server=_Server1, name=DbName}) ->
    delete(Server, DbName);
delete(#server{url=ServerUrl, options=Opts}, DbName) ->
    Url = hackney_url:make_url(ServerUrl, couchdb_util:dbname(DbName), []),
    Resp = couchdb_httpc:request(delete, Url, [], <<>>, Opts),
    case couchdb_httpc:db_resp(Resp, [200]) of
        {ok, _, _, Ref} ->
            {ok, couchdb_httpc:json_body(Ref)};
        Error ->
            Error
    end.


%
%
%   BULK
%
%

%% @reference CouchDB Docs 1.3.4
%% @doc NIY: This method can be called to query several documents in bulk. It is well suited for 
%% fetching a specific revision of documents, as replicators do for example, or for getting 
%% revision history.
bulk_get() -> 
    niy.

% NOT TESTED?
%% @reference CouchDB Docs 1.3.5.2
%% @doc save a list of documents
%% @equiv save_docs(Db, Docs, [])
bulk_docs_save(Db, Docs) ->
    bulk_docs_save(Db, Docs, []).
bulk_docs_save(#db{server=Server, options=Opts}=Db, [_Car | _Cdr]=Documents, Options) when is_list(Options) ->    
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Url = hackney_url:make_url(
        couchdb_httpc:server_url(Server),
        [couchdb_httpc:db_url(Db), <<"_bulk_docs">>],
        []
    ),

    Documents1 = lists:map(fun(Document) -> 
        case maps:is_key(<<"_id">>, Document) of
            true -> Document;
            false -> maps:put(<<"_id">>, couchdb_custom:generate_unique_id(), Document)
        end
    end, Documents),

    ReqObj = case Options of
        [{new_edits, Val}] when is_boolean(Val) -> #{<<"docs">> => Documents1, <<"new_edits">> => Val};
        _Other ->  #{<<"docs">> => Documents1}
    end,
    Body = couchdb_ejson:encode(ReqObj),
    
    
    case couchdb_httpc:db_request(post, Url, Headers, Body, Opts, [201]) of
    {ok, _, _, Ref} ->
        {ok, couchdb_httpc:json_body(Ref)};
    Error ->
        Error
    end.

% NOT TESTED
%% @reference CouchDB Docs 1.3.15
%% @doc Commits any recent changes to the specified database to disk. You should call this if you want to ensure that recent changes have been flushed. This function is likely not required, assuming you have the recommended configuration setting of delayed_commits=false, which requires CouchDB to ensure changes are written to disk before a 200 or similar result is returned.
%% @equiv ensure_full_commit(Db, [])
ensure_full_commit(Db) ->
    ensure_full_commit(Db, []).

%% @doc Commits any recent changes to the specified database to disk. You should call this if you want to ensure that recent changes have been flushed. This function is likely not required, assuming you have the recommended configuration setting of delayed_commits=false, which requires CouchDB to ensure changes are written to disk before a 200 or similar result is returned.
-spec ensure_full_commit(Db::db(), Options::list()) -> {ok, InstancestartTime :: binary()} | {error, term()}.
ensure_full_commit(#db{server=Server, options=Opts}=Db, Options) ->
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Url = hackney_url:make_url(couchdb_httpc:server_url(Server), [couchdb_httpc:db_url(Db), <<"_ensure_full_commit">>], Options),    
    case couchdb_httpc:db_request(post, Url, Headers, <<>>, Opts, [201]) of
        {ok, 201, _, Ref} ->
            #{
                <<"ok">> := true,
                <<"instance_start_time">> := InstanceStartTime} = couchdb_httpc:json_body(Ref),
            {ok, InstanceStartTime};
        Error ->
            Error
    end.

% NOT TESTED
%% @reference CouchDB Docs 1.3.20
%% @doc Given a list of document revisions, returns the document revisions that do not exist in the database.
-spec get_missing_revs(#db{}, [{binary(), [binary()]}]) -> {ok, [{DocId :: binary(), [MissingRev :: binary()], [PossibleAncestor :: binary()]}]} | {error, term()}.
get_missing_revs(#db{server=Server, options=Opts}=Db, IdRevs) ->
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Url = hackney_url:make_url(couchdb_httpc:server_url(Server), [couchdb_httpc:db_url(Db), <<"_revs_diff">>],[]),
    Json = couchdb_ejson:encode({IdRevs}),

    case couchdb_httpc:db_request(post, Url, Headers, Json, Opts, [200]) of
        {ok, _RespCode, _, Ref} ->
            Listing = couchdb_httpc:json_body(Ref),
            {ok, Listing};
        Error -> Error
    end.

