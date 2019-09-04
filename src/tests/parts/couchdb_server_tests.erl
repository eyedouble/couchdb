-module(couchdb_server_tests).

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


info_test() ->
    Server = init(),
    Res = couchdb_server:info(Server),
    ?assertMatch({ok, #{
        <<"couchdb">> := <<"Welcome">>, 
        <<"features">> :=  _, 
        <<"git_sha">> := _,
        <<"uuid">> := _,
        <<"vendor">> := #{<<"name">> := <<"The Apache Software Foundation">>},
        <<"version">> := _
    }}, Res).

all_dbs_test() -> 
    Server = init(),
    % TODO CREATE DBs FIRST
    Res = couchdb_server:all_dbs(Server),
    ?assertMatch({ok, []}, Res).

% replicate_test() ->
%     Server = init(),

   
%     {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
%     {ok, Db2} = couchdb_databases:create(Server, ?MOCK_DBS(2)),

%     ?PRINT(Db),

%     {ok, Doc11} = couchdb:save_doc(Db, {[]}),
%     DocId11 = couchdb_doc:get_id(Doc11),
%     DocRev11 = couchdb_doc:get_rev(Doc11),
%     ?assertMatch({ok, _}, couchdb:replicate(Server, Db, Db2)),

%     {ok, Doc11_2} = couchdb:open_doc(Db2, DocId11),
%     DocRev11_2 = couchdb_doc:get_rev(Doc11_2),
%     ?assertEqual(DocRev11_2, DocRev11),

%     {ok, Doc12} = couchdb:save_doc(Db, Doc11 ),
%     {ok, Doc13} = couchdb:save_doc(Db, Doc12),

%     DocRev12 = couchdb_doc:get_rev(Doc12),
%     DocRev13 = couchdb_doc:get_rev(Doc13),
%     {ok, Missing} = couchdb:get_missing_revs(Db2, [{DocId11, [DocRev12,
%                                                                 DocRev13]}]),
%     ?assertEqual([{DocId11, [DocRev12, DocRev13], [DocRev11]}], Missing),

%     {ok, InstanceStartTime} = couchdb:ensure_full_commit(Db),
%     ?assert(is_binary(InstanceStartTime)),
%     ok.