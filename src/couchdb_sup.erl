%%% -*- erlang -*-
%%%
%%% This file is part of couchdb released under the MIT license.
%%% See the NOTICE for more information.

-module(couchdb_sup).
-author('Benoît Chesneau <benoitc@e-engura.org>').
-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    %% gen_server to cache UUIDs generated by the couchdb node.
    Uuids = {couchdb_uuids,
             {couchdb_uuids, start_link, []},
             permanent,2000,worker, [couchdb_uuids]},

    %% view stream supervisor
    ViewSup = {couchdb_view_sup,
               {couchdb_view_sup, start_link, []},
               permanent, 2000, supervisor, [couchdb_view_sup]},

    %% changes stream supervisor
    ChangesSup = {couchdb_changes_sup,
                  {couchdb_changes_sup, start_link, []},
                  permanent, 2000, supervisor, [couchdb_changes_sup]},

    {ok, {{one_for_one, 10, 3600}, [Uuids, ViewSup, ChangesSup]}}.
