%% @doc The `couchdb' module contains functionality specic to this package.
%% It is a series of helper/quality-of-life functions
%% that do not directly reflect any functionality in the offical CouchDB Documentation.
%%
%%
-module(couchdb).
-author('Irsan van Wel <vanwel@eyedouble.nl>').

-include("couchdb.hrl").

-define(TIMEOUT, infinity).


-export([
    server_record/1
    ,server_record/2
    ,database_record/2
    ,database_record/3
    ,generate_unique_id/0
    ,get_document_id/1
    ,get_document_rev/1
    ,database_name_is_valid/1
]).

%% --------------------------------------------------------------------
%% CouchDB Erlang/Elixir Specific
%% --------------------------------------------------------------------


%% %reference Custom API
%% @doc Create a server record, it is used to connect to CouchDB;
%% Server releated functions take this record as an argument.
%% This record is required to make a database record.
%% @equiv server_record/3
-spec(server_record(Url::binary()) -> Server::server()).
server_record(<<Url/binary>>) -> server_record(Url, []).

%% %reference Custom API
%% @doc Create a server record, it is used to connect to CouchDB;
%% Server releated functions take this record as an argument.
%% This record is required to make a database record.
%% 
%% Options() = [option()]
%%      option() =
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
%%      username() = string()
%%      password() = string()
%%      SSLOpt = term()
%%      oauthOptions() = [oauth()]
%%      oauth() =
%%          {consumer_key, string()} |
%%          {token, string()} |
%%          {token_secret, string()} |
%%          {consumer_secret, string()} |
%%          {signature_method, string()}
%%
%%      proxyOpt = {X-Auth-CouchDB-UserName, username :: string()} |
%%           {X-Auth-CouchDB-Roles, roles :: string} | list_of_user_roles_separated_by_a_comma
%%           {X-Auth-CouchDB-Token: token :: string()} | authentication token. Optional, but strongly recommended to force token be required to prevent requests from untrusted sources.
-spec(server_record(Url::binary(), OptionsList::list()) -> Server::server()).
server_record(<<Url/binary>>=Url, Options) when is_list(Options) -> 
    Record = fun(Url1, Options1) ->
        {ok, #server{url=hackney_url:fix_path(Url1), options=Options1}}
    end,

    case tuplelist_is_compliant(Options) of
        true -> case re:split(Url, <<":">>) of
            [<<"https">>, <<"//", _Path/binary>>, <<_PortRest/binary>>] -> Record(Url, Options);
            [<<"http">>, <<"//", _Path/binary>>, <<_PortRest/binary>>] -> Record(Url, Options);
            _Other -> {error, "Invalid url provided"}  
        end;
        false -> {error, "Options must be a valid tuplelist"} 
    end.


%% %reference Custom API
%% @doc Create a database record, it is used to connect to CouchDB;
%% Database and Document releated functions take this record as an argument.
%% @equiv database_record/3
database_record(#server{options=_ServerOptions}=Server, <<DatabaseName/binary>>)  -> database_record(Server, DatabaseName, []).

%% %reference Custom API
%% @doc Create a database record, it is used to connect to CouchDB;
%% Database and Document releated functions take this record as an argument.
-spec(database_record(Server::server(), DbName::binary(), OptionsList::list())-> {ok, db()} | {error, term()}).
database_record(#server{options=ServerOptions}=Server, <<DatabaseName/binary>>, DatabaseOptions) when is_list(DatabaseOptions) ->
    case
        tuplelist_is_compliant(ServerOptions) 
        andalso tuplelist_is_compliant(DatabaseOptions) 
        andalso database_name_is_valid(DatabaseName) of
            true -> 
                Options = ServerOptions ++ DatabaseOptions,
                {ok, #db{server=Server, name=DatabaseName, options=Options}};
            false -> {error, "Options must be a valid tuplelist"} 
    end.

%% %reference Custom API
%% @doc Returns the Id of a Document
-spec generate_unique_id() -> binary().
generate_unique_id() ->
    generate_uuid_v4().

%% %reference Custom API
%% @doc Returns the Id of a Document
-spec get_document_id(map()) -> binary() | atom().
get_document_id(#{<<"_id">> := <<Id/binary>>}=_Document) -> Id;
get_document_id(_Other) -> undefined.

%% %reference Custom API
%% @doc Returns the Rev of a Document
-spec(get_document_rev(map()) -> binary() | atom()).
get_document_rev(#{<<"_rev">> := <<Rev/binary>>}=_Document) -> Rev;
get_document_rev(_Other) -> undefined.

%% %reference Custom API
%% @doc Checks if the name of the database is valid as per CouchDB API Docs 1.3.1/PUT
-spec(database_name_is_valid(binary()) -> boolean()).
database_name_is_valid(<<DatabaseName/binary>>) ->
    case re:run(DatabaseName, "^[a-z][a-z0-9_$()+/-]*$") of
        {match, _} -> true;
        nomatch -> false
    end;
database_name_is_valid(_) -> false.


%
%   PRIVATE
%
%% @private
generate_uuid_v4() -> couchdb_uuids:new().

%% Checks if every element in the list is a key/value pair tuple 
%% and if the key of such a tuple is an `atom` or `bitstring`.
%% @private
-spec(tuplelist_is_compliant(list()) -> boolean()).
tuplelist_is_compliant(TupleList) ->
    length([{Key, Value} || {Key, Value} <- TupleList, is_atom(Key) orelse is_binary(Value)]) =:= length(TupleList).
