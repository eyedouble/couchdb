-module(couchdb_databases_tests).

-include_lib("eunit/include/eunit.hrl").

-include ( "../src/dev.hrl" ).
-include("../test_helper.hrl").


%
%   HELPER
%
init() ->
    {ok, _} = application:ensure_all_started(couchdb),
    Server = couchdb_custom:server_record(<<"http://localhost:5984">>),
    [ catch couchdb_databases:delete(Server, MockDb) || MockDb <- ?MOCK_DBS ],
    Server.

%
%   TESTS
%
exists_test() ->
    {ok, Server} = init(),
    Res0 = couchdb_databases:exists(Server, ?MOCK_DBS(1)),
    ?assertEqual(false, Res0),
    couchdb_databases:create(Server, ?MOCK_DBS(1)),
    Res1 = couchdb_databases:exists(Server, ?MOCK_DBS(1)),
    ?assertEqual(true, Res1).

create_test() -> 
    {ok, Server} = init(),
    {ok, Database} = couchdb_custom:database_record(Server, ?MOCK_DBS(1)),
    {ok, Database2} = couchdb_custom:database_record(Server, ?MOCK_DBS(2)),
    ?assertMatch({ok, _}, couchdb_databases:create(Database)),
    ?assertEqual({error, db_exists}, couchdb_databases:create(Database)),
    ?assertMatch({ok, _}, couchdb_databases:create(Database2)).

delete_test() ->
    {ok, Server} = init(),
    {ok, Database} = couchdb_custom:database_record(Server, ?MOCK_DBS(1)),
    {ok, Database} = couchdb_databases:create(Database),    
    Res = couchdb_databases:delete(Server, Database),
    ?assertMatch({ok,#{<<"ok">> := true}}, Res).