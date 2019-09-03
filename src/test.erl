-module(test).

-include("couchdb.hrl").

-define(ASSERT(X, Y), X =:= Y ).

-export([
    main/0
]).

main() ->
    S = couchdb:server_connection(<<"http://localhost:5984">>),
    {ok, Db} = couchdb:open_db(S, <<"testdb">>),
    couchdb:save_doc(Db, #{<<"zzz">> => <<"lalaal">>}).

test_open_db() ->
    S = couchdb:server_connection(<<"http://localhost:5984">>),
    {ok, Db} = couchdb:open_db(S, <<"testdb">>),
    ?ASSERT(Db, {db,{server,<<"http://localhost:5984">>,[]},
        <<"testdb">>,[]}).


test_create_db() ->
     S = couchdb:server_connection(<<"http://localhost:5984">>),
     R = couchdb:create_db(S, <<"testdb">>),
     ?ASSERT(R, {ok,{db,{server,<<"http://localhost:5984">>,[]},
        <<"testdb">>,[]}}).

test_server_info() ->
    S = couchdb:server_connection(<<"http://localhost:5984">>),
    R = couchdb:server_info(S),
    ?ASSERT(R, {ok,#{<<"couchdb">> => <<"Welcome">>,
      <<"features">> =>
          [<<"pluggable-storage-engines">>,<<"scheduler">>],
      <<"git_sha">> => <<"c298091a4">>,
      <<"uuid">> => <<"37ec78ad80618a1deedf56da7cd2c1d9">>,
      <<"vendor">> => #{<<"name">> => <<"eyedouble">>},
      <<"version">> => <<"2.3.1">>}}).
