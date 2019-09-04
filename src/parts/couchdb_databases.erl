-module(couchdb_databases).

-include("couchdb.hrl").

-export([
    exists/2
    ,info/1
    ,create/2
    ,create/3
    ,create/4
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



