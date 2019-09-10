%% @hidden
-module(couchdb_app).

-behaviour(application).

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
    application:ensure_all_started(hackney),
    couchdb_util:start_app_deps(couchdb),
    couchdb_sup:start_link().

%%--------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%--------------------------------------------------------------------
stop(_State) ->
    ok.
