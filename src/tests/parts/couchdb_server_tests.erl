-module(couchdb_server_tests).

-include_lib("eunit/include/eunit.hrl").

-include ( "../src/dev.hrl" ).
-include("../test_helper.hrl").


%
%   HELPERS
%

clean_dbs() ->
    Server = couchdb:server_connection(),
    [ catch couchdb_databases:delete(Server, MockDb) || MockDb <- ?MOCK_DBS ],
    % timer:sleep(300),
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


info_test() ->
    Server = init(),
    Res = couchdb_server:info(Server),
    ?assertMatch({ok, #{
        <<"couchdb">> := <<"Welcome">>,  
        <<"uuid">> := _,
        <<"vendor">> := _,
        <<"version">> := _
    }}, Res).

all_dbs_test() -> 
    Server = init(),
    % TODO CREATE DBs FIRST
    {ok, ListOfDbs} = couchdb_server:all_dbs(Server),
    ?assert(is_list(ListOfDbs)).


replicate_test() ->
    Server = init(),
   
    {ok, DbA} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    {ok, DbB} = couchdb_databases:create(Server, ?MOCK_DBS(2)),

    {ok, #{
        <<"_id">> := <<DocA1Id/binary>>,
        <<"_rev">> := <<DocA1Rev/binary>>
    }=DocA1} = couchdb_documents:save(DbA, ?MOCK_DOCS(1)),


    {ok, #{
        <<"_id">> := <<DocA2Id/binary>>,
        <<"_rev">> := <<DocA2Rev/binary>>
    }=DocA2} = couchdb_documents:save(DbA, ?MOCK_DOCS(2)),
    
    ?assertMatch({ok, _}, couchdb_server:replicate(Server, DbA, DbB)),


    {ok, DocB1} = couchdb_documents:get(DbB, DocA1Id),
    DocB1Rev = couchdb_custom:get_document_rev(DocB1),
    ?assertEqual(DocA1Rev, DocB1Rev),

    {ok, DocA1_0} = couchdb_documents:save(DbA, DocA1),
    {ok, DocA1_1} = couchdb_documents:save(DbA, DocA1_0),

    DocA1_0Rev = couchdb_custom:get_document_rev(DocA1_0),
    DocA1_1Rev = couchdb_custom:get_document_rev(DocA1_1),

    % Test 
    {ok, DocA2_0} = couchdb_documents:save(DbA, DocA2),
    {ok, DocA2_1} = couchdb_documents:save(DbA, DocA2_0),

    DocA2_0Rev = couchdb_custom:get_document_rev(DocA2_0),
    _DocA2_1Rev = couchdb_custom:get_document_rev(DocA2_1),

    {ok, MissingObj} = couchdb_databases:get_missing_revs(
        DbB, 
        [
            {DocA1Id, [DocA1_0Rev, DocA1_1Rev]}, 
            {DocA2Id, [DocA2_0Rev, DocA2Rev]}
        ]
    ),

    ?assertMatch(#{
            <<"missing">> := [DocA1_0Rev, DocA1_1Rev],
            <<"possible_ancestors">> := [DocA1Rev]
    }, maps:get(DocA1Id, MissingObj, nil)),

    ?assertMatch(#{
            <<"missing">> := [DocA2_0Rev],
            <<"possible_ancestors">> := [DocA2Rev]
    }, maps:get(DocA2Id, MissingObj, nil)),


    {ok, InstanceStartTime} = couchdb_databases:ensure_full_commit(DbA),
    ?assert(is_binary(InstanceStartTime)),
    ok.