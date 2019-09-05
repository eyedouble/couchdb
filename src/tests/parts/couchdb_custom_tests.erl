-module(couchdb_custom_tests).

-include_lib("eunit/include/eunit.hrl").

-include ( "../src/dev.hrl" ).
-include("../test_helper.hrl").


%
%   HELPERS
%

clean_dbs() ->
    Server = couchdb:server_connection(),
    [ catch couchdb:delete_db(Server, MockDb) || MockDb <- ?MOCK_DBS ],
    % timer:sleep(100),
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

get_document_id_test() ->
    ?assertEqual(undefined, couchdb_custom:get_document_id(?MOCK_DOCS(1))),   
    DocWithIdAndRev = maps:merge(?MOCK_DOCS(1), #{<<"_id">> => <<"fakeid">>, <<"_rev">> => <<"fakerev">>}),
    ?assertEqual(<<"fakeid">>, couchdb_custom:get_document_id(DocWithIdAndRev)).

get_document_rev_test() ->
    ?assertEqual(undefined, couchdb_custom:get_document_rev(?MOCK_DOCS(1))),   
    DocWithIdAndRev = maps:merge(?MOCK_DOCS(1), #{<<"_id">> => <<"fakeid">>, <<"_rev">> => <<"fakerev">>}),   
    ?assertEqual(<<"fakerev">>, couchdb_custom:get_document_rev(DocWithIdAndRev)).