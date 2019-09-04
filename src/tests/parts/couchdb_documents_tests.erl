-module(couchdb_documents_tests).

-include_lib("eunit/include/eunit.hrl").

-include ( "../src/dev.hrl" ).
-include("../test_helper.hrl").


%
%   HELPERS
%

clean_dbs() ->
    Server = couchdb:server_connection(),
    [ catch couchdb:delete_db(Server, MockDb) || MockDb <- ?MOCK_DBS ],
    timer:sleep(100),
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

create_without_id_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    Doc = couchdb_documents:save(Db, ?MOCK_DOCS(1)), % {[{<<"test">>, <<"blah">>}]}
    ?assertMatch(#{
        <<"content">> := <<"Example Content">>,
        <<"id">> := <<_/binary>>,
        <<"rev">> := <<"1-", _/binary>>,
        <<"title">> := <<"Untitled 1">>
    }, Doc).