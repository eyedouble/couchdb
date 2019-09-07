%%% -*- erlang -*-
%%%
%%% This file is part of couchdb released under the MIT license.
%%% See the NOTICE for more information.

-module(couchdb_changes_sup).

 -behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->

    %% start table to keep async streams ref
    ets:new(couchdb_changes_streams, [set, public, named_table]),

    %% define a stream spec
    Stream = {couchdb_changes_stream,
              {couchdb_changes_stream, start_link, []},
              temporary, infinity, worker, [couchdb_changes_stream]},

    {ok, {{simple_one_for_one, 10, 3600}, [Stream]}}.
