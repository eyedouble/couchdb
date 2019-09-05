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
    % timer:sleep(50),
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
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    ?assertMatch(false, couchdb_documents:exists(Db, <<"fakeid">>)),
    {ok, #{<<"_id">> := Id}=_Doc} = couchdb_documents:save(Db, ?MOCK_DOCS(1)),
    ?assertMatch(true, couchdb_documents:exists(Db, Id)).


get_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    ?assertEqual({error, not_found}, couchdb_documents:get(Db, <<"test2">>)),

    {ok, #{<<"_id">> := Id}=Doc} = couchdb_documents:save(Db, ?MOCK_DOCS(1)),
    ?assertMatch({ok, #{
        <<"_id">> := Id,
        <<"_rev">> := <<"1-", _Rev/binary>>
    }}, couchdb_documents:get(Db, Id)).
 
    
    % {ok, Doc} = couchdb:save_doc(Db, {[{<<"test">>, <<"blah">>}]}),
    % ?assertMatch({_}, Doc),
    % {ok, {Props}} = couchdb:save_doc(Db, {[{<<"_id">>,<<"test">>}, {<<"test">>,<<"blah">>}]}),
    % ?assertEqual(<<"test">>, proplists:get_value(<<"_id">>, Props)),
    % ?assertEqual({error, conflict}, couchdb:save_doc(Db, {[{<<"_id">>,<<"test">>}, {<<"test">>,<<"blah">>}]})),

    % Rev = couchdb:lookup_doc_rev(Db, "test"),
    % {ok, {Doc1}} = couchdb:open_doc(Db, <<"test">>),
    % ?assertEqual(Rev, proplists:get_value(<<"_rev">>, Doc1)),
    % ?assertEqual(<<"blah">>, proplists:get_value(<<"test">>, Doc1)),

    % _ = couchdb:save_doc(Db, {[{<<"_id">>,<<"test2">>}, {<<"test">>,<<"blah">>}]}),
    % {ok, Doc2} = couchdb:open_doc(Db, "test2"),
    % ?assertMatch({_}, Doc2),
    % ?assertEqual(true, couchdb_doc:is_saved(Doc2)),
    % ?assertEqual(<<"test2">>, couchdb_doc:get_id(Doc2)),
    % ?assertMatch(true, couchdb:doc_exists(Db, "test2")),
    % ?assertMatch({ok, _}, couchdb:delete_doc(Db, Doc2)),
    % ?assertEqual({error, not_found}, couchdb:open_doc(Db, "test2")),
    % ?assertMatch(false, couchdb:doc_exists(Db, "test2")),

    % Doc3 = {[{<<"_id">>, <<"~!@#$%^&*()_+-=[]{}|;':,./<> ?">>}]},
    % {ok, _Doc4} = couchdb:save_doc(Db, Doc3),
    % {ok, Doc5} = couchdb:open_doc(Db, <<"~!@#$%^&*()_+-=[]{}|;':,./<> ?">>),
    % ?assertEqual( <<"~!@#$%^&*()_+-=[]{}|;':,./<> ?">>, couchdb_doc:get_id(Doc5)),
    % ok.



save_without_id_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    Doc = couchdb_documents:save(Db, ?MOCK_DOCS(1)), % {[{<<"test">>, <<"blah">>}]}
    ?assertMatch({ok, #{
        <<"content">> := <<"Example Content">>,
        <<"_id">> := <<_/binary>>,
        <<"_rev">> := <<"1-", _/binary>>,
        <<"title">> := <<"Untitled 1">>
    }}, Doc).



delete_test() -> 
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    Doc = couchdb_documents:save(Db, ?MOCK_DOCS(1)),
    ?assertMatch({ok, _}, couchdb_documents:delete(Db, Doc)).