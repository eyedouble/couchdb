%%% -*- erlang -*-
%%%
%%% This file is part of couchdb released under the MIT license.
%%% See the NOTICE for more information.

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
    couchdb_util:start_app_deps(couchdb),
    couchdb_sup:start_link().

%%--------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%--------------------------------------------------------------------
stop(_State) ->
    ok.
