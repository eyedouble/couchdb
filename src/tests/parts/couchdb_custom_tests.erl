-module(couchdb_custom_tests).

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
server_record_test() -> 
    Res0 = couchdb_custom:server_record(<<"http://localhost:5984">>),
    Res1 = couchdb_custom:server_record(<<"https://localhost:5984">>, [{basic_auth, {<<"username">>, <<"password">>}}]),
    Res2 = couchdb_custom:server_record(<<"http://localhost">>),
    ?assertEqual({server,<<"http://localhost:5984">>,[]}, Res0),
    ?assertEqual({server,<<"https://localhost:5984">>,[{basic_auth, {<<"username">>, <<"password">>}}]}, Res1),
    ?assertMatch({error,_}, Res2).

get_document_id_test() ->
    ?assertEqual(undefined, couchdb_custom:get_document_id(?MOCK_DOCS(1))),   
    DocWithIdAndRev = maps:merge(?MOCK_DOCS(1), #{<<"_id">> => <<"fakeid">>, <<"_rev">> => <<"fakerev">>}),
    ?assertEqual(<<"fakeid">>, couchdb_custom:get_document_id(DocWithIdAndRev)).

get_document_rev_test() ->
    ?assertEqual(undefined, couchdb_custom:get_document_rev(?MOCK_DOCS(1))),   
    DocWithIdAndRev = maps:merge(?MOCK_DOCS(1), #{<<"_id">> => <<"fakeid">>, <<"_rev">> => <<"fakerev">>}),   
    ?assertEqual(<<"fakerev">>, couchdb_custom:get_document_rev(DocWithIdAndRev)).