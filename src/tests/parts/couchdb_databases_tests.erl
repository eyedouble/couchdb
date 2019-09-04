-module(couchdb_databases_tests).

-include_lib("eunit/include/eunit.hrl").

-include ( "../src/dev.hrl" ).
-include("../test_helper.hrl").


%
%   HELPERS
%

clean_dbs() ->
    Server = couchdb:server_connection(),
    [ catch couchdb:delete_db(Server, MockDb) || MockDb <- ?MOCK_DBS ],
    timer:sleep(300),
    ok.

start_couchdb_tests() ->
    {ok, _} = application:ensure_all_started(couchdb),
    clean_dbs().

init() ->
    start_couchdb_tests(),
    couchdb:server_connection(<<"http://localhost:5984">>).

%
%   TESTS
%
exists_test() ->
    Server = init(),
    Res0 = couchdb_databases:exists(Server, ?MOCK_DBS(1)),
    ?assertEqual(false, Res0),
    couchdb_databases:create(Server, ?MOCK_DBS(1)),
    Res1 = couchdb_databases:exists(Server, ?MOCK_DBS(1)),
    ?assertEqual(true, Res1).

create_test() -> 
    Server = init(),
    ?assertMatch({ok, _}, couchdb_databases:create(Server, ?MOCK_DBS(1))),
    ?assertEqual({error, db_exists}, couchdb_databases:create(Server, ?MOCK_DBS(1))),
    ?assertMatch({ok, _}, couchdb_databases:create(Server, ?MOCK_DBS(2))).