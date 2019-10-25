%% @doc The `couchdb_mango' module contains functionality listed under CouchDB API
%% Reference section 1.3.6.
%%
%% Now let’s do something a little more useful: create databases. For the strict, CouchDB 
%% is a database management system (DMS). That means it can hold multiple databases. A 
%% database is a bucket that holds "related data".
%%
-module(couchdb_databases).

-include("couchdb.hrl").
-include("../../dev.hrl").

-export([
    exists/1
    ,exists/2
    ,info/1
    ,create/1
    ,create/2
    ,delete/1
    ,delete/2
    ,all_docs/1
    ,all_docs/2
    ,bulk_docs_save/2
    ,bulk_docs_save/3
    ,compact/1
    ,compact/2
    ,ensure_full_commit/1
    ,ensure_full_commit/2
    ,get_missing_revs/2
]).

%% @doc Returns true if database exists, false if not
%%
%% Uses HTTP Headers containing a minimal amount of information about the specified db
%% @equiv exists(Server, DatabaseName)
-spec(exists(Database::db()) -> boolean()).
exists(#db{server=#server{}=Server, name=DbName}) -> exists(Server, DbName).

%% %reference CouchDB Docs 1.3.1/HEAD
%% @doc Returns true if database exists, false if not

%% Returns the HTTP Headers containing a minimal amount of information about the specified 
%% database. Since the response body is empty, using the HEAD method is a lightweight way to 
%% check if the database exists already or not.
-spec(exists(Server::server(), Name::binary()) -> boolean()).
exists(#server{url=ServerUrl, options=Opts}, <<DbName/binary>>) ->
    case couchdb:database_name_is_valid(DbName) of
        true -> 
            Url = hackney_url:make_url(ServerUrl, DbName, []),
            case couchdb_httpc:db_request(head, Url, [], <<>>, Opts, [200]) of
                {ok, 200, _}->
                    true;
                _Error ->
                    false
            end;
        false -> false
    end.

%% %reference CouchDB Docs 1.3.1/GET
%% @doc get database info
-spec(info(Database::db()) -> {ok, binary()} | {error, term()}).
info(#db{server=Server, name=DbName, options=Opts}) ->
    Url = hackney_url:make_url(couchdb_httpc:server_url(Server), DbName, []),
    case couchdb_httpc:db_request(get, Url, [], <<>>, Opts, [200]) of
        {ok, _Status, _Headers, Ref} ->
            Infos = couchdb_httpc:json_body(Ref),
            {ok, Infos};
        {error, not_found} ->
            {error, db_not_found};
       Error ->
          Error
    end.

%% %reference CouchDB Docs 1.3.1/PUT
%% @doc Create a database
%%
%%      Connections are made to:
%%      ```http://Host:PortPrefix/DbName'''
%%
%% If ssl is set https is used. See server_connections for options.
%% Params is a list of optionnal query argument you want to pass to the
%% db. Useful for bigcouch for example.
%% DB names must conform to ^[a-z][a-z0-9_$()+/-]*$

-spec(create(Database::db(), Params::list()) -> {ok, db()} | {error, term()}).
create(#db{server=#server{url=ServerUrl}, name=DbName, options=Options}=Db, Params) ->    
    Url = hackney_url:make_url(ServerUrl, DbName, Params),
    Resp = couchdb_httpc:db_request(put, Url, [], <<>>, Options, [201]),
    case Resp of
        {ok, _Status, _Headers, Ref} ->
            hackney:skip_body(Ref),
            {ok, Db};
        {error, precondition_failed} ->
            {error, db_exists};
       Error ->
          Error
    end;

%% %reference CouchDB Docs 1.3.1/PUT
%% @doc Create a database 
%% @equiv create(Db, [])
create(#server{}=Server, <<DbName/binary>>) -> 
    {ok, Db} = couchdb:database_record(Server, DbName),
    create(Db, []).

%% %reference CouchDB Docs 1.3.1/PUT
%% @doc Create a database 
%% @equiv create(Db, [])
-spec(create(Database::db()) -> {ok, db()} | {error, term()}).
create(#db{}=Database) -> create(Database, []).


%% %reference CouchDB Docs 1.3.1/DELETE
%% @doc delete database   
-spec(delete(Database::db()) -> {ok, db()} | {error, term()}).
delete(#db{server=#server{url=ServerUrl}, name=DbName, options=Opts}) ->
    Url = hackney_url:make_url(ServerUrl, DbName, []),
    Resp = couchdb_httpc:request(delete, Url, [], <<>>, Opts),
    case couchdb_httpc:db_resp(Resp, [200]) of
        {ok, _, _, Ref} ->
            {ok, couchdb_httpc:json_body(Ref)};
        Error ->
            Error
    end.

%% %reference CouchDB Docs 1.3.1/DELETE
%% @equiv delete(Db) 
-spec(delete(Server::server(), DbName::binary()) -> {ok, db()} | {error, term()}).
delete(#server{url=_ServerUrl, options=_Opts}=Server, <<DbName/binary>>) ->
        {ok, Db} = couchdb:database_record(Server, DbName),
        delete(Db).




%% %reference CouchDB Docs 1.3.1/DELETE
%% @equiv all_docs(Db, #{}) 
-spec(all_docs(Database::db()) -> {ok, binary()} | {error, term()}).
all_docs(#db{server=Server, name=_DbName, options=_DbOpts}=Db) -> all_docs(Db, #{}).

%% %reference CouchDB Docs 1.3.2/GET
%% @doc All Docs
%% 
%% Options:
%% - include_docs (true|false)
%%
-spec(all_docs(Database::db(), Options::map()) -> {ok, binary()} | {error, term()}).
all_docs(#db{server=Server, name=_DbName, options=DbOpts}=Db, #{}=Options) ->
    QueryParams = case Options of
        #{<<"include_docs">> := true} -> <<"include_docs=true">>;
        _ -> []
    end,
    Url = hackney_url:make_url(
        couchdb_httpc:server_url(Server),
        [couchdb_httpc:db_url(Db), <<"_all_docs">>],
        QueryParams
    ),
    case couchdb_httpc:db_request(get, Url, [], <<>>, DbOpts, [200]) of
        {ok, _Status, _Headers, Ref} ->
            Docs = couchdb_httpc:json_body(Ref),
            {ok, Docs};
        {error, not_found} ->
            {error, db_not_found};
       Error ->
          Error
    end.



%
%   BULK
%

%% %reference CouchDB Docs 1.3.4
%% @doc NIY: This method can be called to query several documents in bulk. It is well suited for 
%% fetching a specific revision of documents, as replicators do for example, or for getting 
%% revision history.
bulk_get() -> niy.


%% %reference CouchDB Docs 1.3.5.2
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
            false -> maps:put(<<"_id">>, couchdb:generate_unique_id(), Document)
        end
    end, Documents),

    ReqObj = case Options of
        [{new_edits, Val}] when is_boolean(Val) -> #{<<"docs">> => Documents1, <<"new_edits">> => Val};
        _Other ->  #{<<"docs">> => Documents1}
    end,
    Body = couchdb_ejson:encode(ReqObj),
    
    
    case couchdb_httpc:db_request(post, Url, Headers, Body, Opts, [201]) of
    {ok, 201, _, Ref} ->    
        Response = couchdb_httpc:json_body(Ref),       
        BulkList =  [ maps:merge(Doc0, #{<<"_id">> => Id1, <<"_rev">> => Rev})
                    || #{<<"_id">> := Id0}=Doc0 <- Documents1,
                    #{<<"id">> := Id1, <<"rev">> := Rev, <<"ok">> := true}=_Doc1 <- Response,
                    Id0 =:= Id1],        
        {ok, BulkList};
    Error ->
        Error
    end.

% NOT TESTED
%% %reference CouchDB Docs 1.3.13
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

% NOT TESTED
%% %reference CouchDB Docs 1.3.14
%% @doc Like compact/1 but this compacts the view index from the
%% current version of the design document.
%% See [http://wiki.apache.org/couchdb/Compaction#View_compaction] for more informations
%% @spec compact(Db::db(), ViewName::string()) -> ok|{error, term()}
compact(#db{server=Server, options=Opts}=Db, DesignName) ->
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Url = hackney_url:make_url(
        couchdb_httpc:server_url(Server), [couchdb_httpc:db_url(Db), <<"_compact">>, DesignName], []
    ),    
    case couchdb_httpc:db_request(post, Url, Headers, <<>>, Opts, [202]) of
        {ok, _, _, Ref} ->
            hackney:skip_body(Ref),
            ok;
        Error ->
            Error
    end.

% NOT TESTED
%% %reference CouchDB Docs 1.3.15
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
%% %reference CouchDB Docs 1.3.20
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

