-module(couchdb_server).

-include("couchdb.hrl").

-export([
    info/1
    ,all_dbs/1
    ,all_dbs/2
    ,replicate/2
    ,replicate/3
    ,replicate/4
]).

%% %reference CouchDB Docs 1.2.1
%% @doc Get Information from the server
%% @spec info(server()) -> {ok, iolist()}
info(#server{url=Url, options=Opts}) ->
    case hackney:get(Url, [], <<>>, Opts) of
        {ok, 200, _, Ref} ->
            Version = couchdb_httpc:json_body(Ref),
            {ok, Version};
        {ok, Status, Headers, Ref} ->
            {ok, Body} = hackney:body(Ref),
            {error, {bad_response, {Status, Headers, Body}}};

        Error ->
            Error
    end.

%% %reference CouchDB Docs 1.2.2
%% @doc List of running tasks, including the task type, name, status and process ID. 
active_tasks() -> niy.

%% %reference CouchDB Docs 1.2.3
%% @doc get list of databases on a CouchDB node
%% @spec all_dbs(server()) -> {ok, iolist()}
all_dbs(#server{}=Server) -> all_dbs(Server, []).

%% @doc get list of databases on a CouchDB node with optional filter
%% @spec all_dbs(server(), view_options()) -> {ok, iolist()}
all_dbs(#server{url=ServerUrl, options=Opts}, Options) ->
    Args = couchdb_view:parse_view_options(Options),
    Url = hackney_url:make_url(ServerUrl, <<"_all_dbs">>, Args#view_query_args.options),
    Resp = couchdb_httpc:db_request(get, Url, [], <<>>, Opts, [200]),
    case Resp of
        {ok, _, _, Ref} ->
            AllDbs = couchdb_httpc:json_body(Ref),
            {ok, AllDbs};
        Error ->
            Error
    end.

%% %reference CouchDB Docs 1.2.4
%% @doc Returns information of a list of the specified databases in the CouchDB instance. 
dbs_info() -> niy.

%% %reference CouchDB Docs 1.2.5
%% @doc Returns the status of the node or cluster, per the cluster setup wizard. 
cluster_setup() -> niy.

%% %reference CouchDB Docs 1.2.6
%% @doc Returns a list of all database events in the CouchDB instance. 
db_updates() -> niy.

%% %reference CouchDB Docs 1.2.7
%% @doc Displays the nodes that are part of the cluster as cluster_nodes. 
%% The field all_nodes displays all nodes this node knows about, including 
%% the ones that are part of the cluster. The endpoint is useful when setting 
%% up a cluster, see Node Management 
membership() -> niy.

%% %reference CouchDB Docs 1.2.8
%% @doc Request, configure, or stop, a replication operation.
%% It allows to pass for authentication info
%% ```
%% RepObj = {[
%% {<<"source">>, <<"sourcedb">>},
%% {<<"target">>, <<"targetdb">>},
%% {<<"create_target">>, true}
%% ]}
%% replicate(Server, RepObj).
%% '''
%%
-spec(replicate(Server::server(), RepObj::{list()})-> {ok, term()}|{error, term()}).
replicate(#server{url=ServerUrl, options=Opts}, RepObj) ->
    Url = hackney_url:make_url(ServerUrl, [<<"_replicate">>], []),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    JsonObj = couchdb_ejson:encode(RepObj),

     case couchdb_httpc:request(post, Url, Headers, JsonObj, Opts) of
        {ok, Status, _, Ref} when Status =:= 200 orelse Status =:= 201 ->
            Res = couchdb_httpc:json_body(Ref),
            {ok, Res};
        {ok, Status, Headers, Ref} ->
            {ok, Body} = hackney:body(Ref),
            {error, {bad_response, {Status, Headers, Body}}};
        Error ->
            Error
    end.

%% @doc Handle replication.
-spec(replicate(Server::server(), Source::binary(), Target::term()) ->  {ok, term()}|{error, term()}).
replicate(Server, Source, Target) ->
    replicate(Server, Source, Target, []).

%% @doc handle Replication. Allows to pass options with source and
%% target.  Options is a Json object.
%% ex:
%% ```
%% Options = [{<<"create_target">>, true}]}
%% couchdb:replicate(S, "testdb", "testdb2", Options).
%% '''
replicate(Server, Source, Target, {Props}) ->
    replicate(Server, Source, Target, Props);
replicate(Server, #db{name=Source}, Target, Options) ->
    replicate(Server, Source, Target, Options);
replicate(Server, Source, #db{name=Target}, Options) ->
    replicate(Server, Source, Target, Options);
replicate(Server, #db{name=Source}, #db{name=Target}, Options) ->
    replicate(Server, Source, Target, Options);
replicate(Server, Source, Target, Options) ->
    RepProp = [
        {<<"source">>, couchdb_util:to_binary(Source)},
        {<<"target">>, couchdb_util:to_binary(Target)} | Options
    ],
    replicate(Server, {RepProp}).