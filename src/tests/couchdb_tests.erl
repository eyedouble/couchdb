-module(couchdb_tests).

-include ( "../src/dev.hrl" ).
-include_lib("eunit/include/eunit.hrl").


%
%   HELPERS
%

clean_dbs() ->
    Server = couchdb:server_connection(),
    catch couchdb:delete_db(Server, "couchdb_testdb"),
    catch couchdb:delete_db(Server, "couchdb_testdb2"),
    catch couchdb:delete_db(Server, "couchdb_testdb3"),
    timer:sleep(300),
    ok.

start_couchdb_tests() ->
    {ok, _} = application:ensure_all_started(couchdb),
    clean_dbs().


%
%   TESTS
%


% -ifdef(TEST).

% -include_lib("eunit/include/eunit.hrl").
% -include_lib("kernel/include/file.hrl").


% create_db_test() ->
%     start_couchdb_tests(),
%      S = couchdb:server_connection(<<"http://localhost:5984">>),
%      R = couchdb:create_db(S, <<"couchdb_testdb">>),
%      ?assert(R =:= {ok,{db,{server,<<"http://localhost:5984">>,[]},
%         <<"couchdb_testdb">>,[]}}).


% % This function does not check if db exists 
% open_db_test() ->
%     start_couchdb_tests(),
%     S = couchdb:server_connection(<<"http://localhost:5984">>),
%     {ok, Db} = couchdb:open_db(S, <<"fakedb">>),
%     ?assert(Db =:= {db,{server,<<"http://localhost:5984">>,[]},
%         <<"fakedb">>,[]}).

% create_doc_test() ->
%     start_couchdb_tests(),
%     S = couchdb:server_connection(<<"http://localhost:5984">>),
%     {ok, Db} = couchdb:open_db(S, <<"ss">>),
%     R = couchdb:save_doc(Db, #{<<"zzz">> => <<"lalaal">>}),
%     ?PRINT(R),
%     ?assert(R =:= ma).







% basic_test() ->
%     start_couchdb_tests(),
%     Server = couchdb:server_connection(),
%     {ok, {Data}} = couchdb:server_info(Server),
%     ?assertEqual(<<"Welcome">>, proplists:get_value(<<"couchdb">>, Data)),
%     ok.

% db_test() ->
%     start_couchdb_tests(),
%     Server = couchdb:server_connection(),

%     %% test db creation
%     ?assertMatch({ok, _}, couchdb:create_db(Server, "couchdb_testdb")),
%     ?assertEqual({error, db_exists}, couchdb:create_db(Server, "couchdb_testdb")),
%     ?assertMatch({ok, _}, couchdb:create_db(Server, "couchdb_testdb2")),

%     {ok, AllDbs} = couchdb:all_dbs(Server),
%     ?assert(is_list(AllDbs) =:= true),
%     ?assertEqual(true, lists:member(<<"couchdb_testdb">>, AllDbs)),
%     ?assertEqual(true, lists:member(<<"couchdb_testdb2">>, AllDbs)),
%     ?assertEqual(true, couchdb:db_exists(Server, "couchdb_testdb")),
%     ?assertEqual(true, couchdb:db_exists(Server, "couchdb_testdb2")),
%     ?assertMatch({ok, _}, couchdb:delete_db(Server, "couchdb_testdb2")),
%     {ok, AllDbs1} = couchdb:all_dbs(Server),
%     ?assertEqual(true, lists:member(<<"couchdb_testdb">>, AllDbs1)),
%     ?assertEqual(false, lists:member(<<"couchdb_testdb2">>, AllDbs1)),
%     ?assertEqual(true, couchdb:db_exists(Server, "couchdb_testdb")),
%     ?assertEqual(false, couchdb:db_exists(Server, "couchdb_testdb2")),

%     ?assertMatch({ok, _}, couchdb:open_or_create_db(Server, "couchdb_testdb")),
%     ?assertMatch({ok, _}, couchdb:open_or_create_db(Server, "couchdb_testdb2")),
%     ok.

% basic_doc_test() ->
%     start_couchdb_tests(),
%     Server = couchdb:server_connection(),
%     {ok, Db} = couchdb:create_db(Server, "couchdb_testdb"),
%     {ok, Doc} = couchdb:save_doc(Db, {[{<<"test">>, <<"blah">>}]}),
%     ?assertMatch({_}, Doc),
%     {ok, {Props}} = couchdb:save_doc(Db, {[{<<"_id">>,<<"test">>}, {<<"test">>,<<"blah">>}]}),
%     ?assertEqual(<<"test">>, proplists:get_value(<<"_id">>, Props)),
%     ?assertEqual({error, conflict}, couchdb:save_doc(Db, {[{<<"_id">>,<<"test">>}, {<<"test">>,<<"blah">>}]})),

%     Rev = couchdb:lookup_doc_rev(Db, "test"),
%     {ok, {Doc1}} = couchdb:open_doc(Db, <<"test">>),
%     ?assertEqual(Rev, proplists:get_value(<<"_rev">>, Doc1)),
%     ?assertEqual(<<"blah">>, proplists:get_value(<<"test">>, Doc1)),

%     _ = couchdb:save_doc(Db, {[{<<"_id">>,<<"test2">>}, {<<"test">>,<<"blah">>}]}),
%     {ok, Doc2} = couchdb:open_doc(Db, "test2"),
%     ?assertMatch({_}, Doc2),
%     ?assertEqual(true, couchdb_doc:is_saved(Doc2)),
%     ?assertEqual(<<"test2">>, couchdb_doc:get_id(Doc2)),
%     ?assertMatch(true, couchdb:doc_exists(Db, "test2")),
%     ?assertMatch({ok, _}, couchdb:delete_doc(Db, Doc2)),
%     ?assertEqual({error, not_found}, couchdb:open_doc(Db, "test2")),
%     ?assertMatch(false, couchdb:doc_exists(Db, "test2")),

%     Doc3 = {[{<<"_id">>, <<"~!@#$%^&*()_+-=[]{}|;':,./<> ?">>}]},
%     {ok, _Doc4} = couchdb:save_doc(Db, Doc3),
%     {ok, Doc5} = couchdb:open_doc(Db, <<"~!@#$%^&*()_+-=[]{}|;':,./<> ?">>),
%     ?assertEqual( <<"~!@#$%^&*()_+-=[]{}|;':,./<> ?">>, couchdb_doc:get_id(Doc5)),
%     ok.

% bulk_doc_test() ->
%     start_couchdb_tests(),
%     Server = couchdb:server_connection(),
%     {ok, Db} = couchdb:create_db(Server, "couchdb_testdb"),

%     Doc1 = {[{<<"_id">>, <<"a">>}]},
%     Doc2 = {[{<<"_id">>, <<"b">>}]},
%     {ok, [{Props1}, {Props2}]} = couchdb:save_docs(Db, [Doc1, Doc2]),
%     ?assertEqual(<<"a">>, proplists:get_value(<<"id">>, Props1)),
%     ?assertEqual(<<"b">>, proplists:get_value(<<"id">>, Props2)),
%     ?assertMatch(true, couchdb:doc_exists(Db, "a")),
%     ?assertMatch(true, couchdb:doc_exists(Db, "b")),


%     {ok, Doc3} = couchdb:open_doc(Db, <<"a">>),
%     {ok, Doc4} = couchdb:open_doc(Db, <<"b">>),
%     couchdb:delete_docs(Db, [Doc3, Doc4]),
%     ?assertEqual({error, not_found}, couchdb:open_doc(Db, <<"a">>)),
%     ok.

% copy_doc_test() ->
%     start_couchdb_tests(),
%     Server = couchdb:server_connection(),
%     {ok, Db} = couchdb:create_db(Server, "couchdb_testdb"),

%     {ok, Doc} = couchdb:save_doc(Db, {[{<<"test">>, 1}]}),
%     {ok, CopyId, _Rev} = couchdb:copy_doc(Db, Doc),
%     {ok, Doc2} = couchdb:open_doc(Db, CopyId),
%     ?assertEqual(1, couchdb_doc:get_value(<<"test">>, Doc2)),

%     {ok, _Doc3} = couchdb:save_doc(Db, {[{<<"_id">>, <<"test_copy">>}]}),
%     {ok, CopyId1, _} = couchdb:copy_doc(Db, Doc, <<"test_copy">>),
%     ?assertEqual(<<"test_copy">>, CopyId1),
%     {ok, Doc4} = couchdb:open_doc(Db, CopyId1),
%     ?assertEqual(1, couchdb_doc:get_value(<<"test">>, Doc4)),
%     ok.


% attachments_test() ->
%     start_couchdb_tests(),
%     Server = couchdb:server_connection(),
%     {ok, Db} = couchdb:create_db(Server, "couchdb_testdb"),
%     Doc = {[{<<"_id">>, <<"test">>}]},
%     {ok, Doc1} = couchdb:save_doc(Db, Doc),
%     RevDoc1 = couchdb_doc:get_rev(Doc1),
%     {ok, {Res}} = couchdb:put_attachment(Db,"test", "test", "test", [{rev, RevDoc1}]),
%     RevDoc11 = proplists:get_value(<<"rev">>, Res),
%     ?assertNot(RevDoc1 =:= RevDoc11),
%     {ok, Attachment} = couchdb:fetch_attachment(Db, "test", "test"),
%     ?assertEqual( <<"test">>, Attachment),
%     {ok, Doc2} = couchdb:open_doc(Db, "test"),
%     ?assertMatch({ok, {_}}, couchdb:delete_attachment(Db, Doc2, "test")),
%     Doc3 = {[{<<"_id">>, <<"test2">>}]},
%     Doc4 = couchdb_attachments:add_inline(Doc3, "test", "test.txt"),
%     Doc5 = couchdb_attachments:add_inline(Doc4, "test2", "test2.txt"),
%     {ok, _} = couchdb:save_doc(Db, Doc5),
%     {ok, Attachment1} = couchdb:fetch_attachment(Db, "test2", "test.txt"),
%     {ok, Attachment2} = couchdb:fetch_attachment(Db, "test2", "test2.txt"),
%     ?assertEqual( <<"test">>, Attachment1),
%     ?assertEqual( <<"test2">>, Attachment2),
%     {ok, Doc6} = couchdb:open_doc(Db, "test2"),
%     Doc7 = couchdb_attachments:delete_inline(Doc6, "test2.txt"),
%     {ok, _} = couchdb:save_doc(Db, Doc7),
%     ?assertEqual({error, not_found}, couchdb:fetch_attachment(Db, "test2", "test2.txt")),
%     {ok, Attachment4} = couchdb:fetch_attachment(Db, "test2", "test.txt"),
%     ?assertEqual( <<"test">>, Attachment4),
%     {ok, Doc8} = couchdb:save_doc(Db, {[]}),

%     TestFileName = data_path("1M"),
%     {ok, FileInfo} = file:read_file_info(TestFileName),
%     {ok, Fd} = file:open(TestFileName, [read]),
%     FileSize = FileInfo#file_info.size,
%     StreamFun = fun() ->
%                     case file:read(Fd, 4096) of
%                         {ok, Data} ->  {ok, iolist_to_binary(Data)};
%                         _ -> eof
%                     end
%                 end,
%     {ok, _Res2} = couchdb:put_attachment(Db, couchdb_doc:get_id(Doc8), "1M", StreamFun,
%                                           [{content_length, FileSize}, {rev, couchdb_doc:get_rev(Doc8)}]),

%     file:close(Fd),
%     {ok, Doc9} = couchdb:open_doc(Db, couchdb_doc:get_id(Doc8)),
%     Attachements = couchdb_doc:get_value(<<"_attachments">>, Doc9),
%     ?assert(Attachements /= undefined),
%     Attachment5 = couchdb_doc:get_value(<<"1M">>, Attachements),
%     ?assert(Attachment5 /= undefined),
%     ?assertEqual(FileSize, couchdb_doc:get_value(<<"length">>, Attachment5)),
%     {ok, Bin} = couchdb:fetch_attachment(Db, couchdb_doc:get_id(Doc8), "1M"),
%     ?assertEqual(FileSize, iolist_size(Bin)),

%     {ok, _Res3}= couchdb:put_attachment(Db, "test/2", "test", "test"),
%     {ok, Attachment10} = couchdb:fetch_attachment(Db, "test/2", "test"),
%     ?assertEqual(<<"test">>, Attachment10),
%     {ok, _Res4}= couchdb:put_attachment(Db, "test3", "test", "test"),
%     {ok, Attachment11} = couchdb:fetch_attachment(Db, "test3", "test"),
%     ?assertEqual(<<"test">>, Attachment11),
%     ok.

% multipart_test() ->
%     start_couchdb_tests(),
%     Server = couchdb:server_connection(),

%     {ok, Db} = couchdb:create_db(Server, "couchdb_testdb"),

%     {ok, _Res4}= couchdb:put_attachment(Db, "test", "test", "test"),
%     Resp = couchdb:open_doc(Db, <<"test">>, [{attachments, true}]),
%     ?assertMatch( {ok, {multipart, _}}, Resp),

%     {ok, {multipart, Stream}} = Resp,
%     Collected = collect_mp(couchdb:stream_doc(Stream), []),
%     ?assert(proplists:is_defined(doc, Collected)),
%     MpDoc = proplists:get_value(doc, Collected),
%     MpDocId = couchdb_doc:get_id(MpDoc),
%     ?assertEqual(<<"test">>, MpDocId),
%     ?assertEqual(<<"test">>, proplists:get_value(<<"test">>, Collected)),

%     Resp1 = couchdb:open_doc(Db, <<"test">>, [{open_revs, all},
%                                                 {accept, <<"multipart/mixed">>}]),
%     ?assertMatch( {ok, {multipart, _}}, Resp1),
%     {ok, {multipart, Stream1}} = Resp1,
%     Collected1 = collect_mp(couchdb:stream_doc(Stream1), []),
%     MpDoc1 = proplists:get_value(doc, Collected1),
%     MpDocId1 = couchdb_doc:get_id(MpDoc1),
%     ?assertEqual(<<"test">>, MpDocId1),
%     ?assertEqual(<<"test">>, proplists:get_value(<<"test">>, Collected1)),
%     {ok, Doc} = couchdb:save_doc(Db, {[{<<"_id">>, <<"test2">>}]},
%                                        [{<<"test.txt">>, <<"test">>}], []),
%     ?assertEqual(<<"test2">>, couchdb_doc:get_id(Doc)),
%     {ok, MpAttachment1} = couchdb:fetch_attachment(Db, <<"test2">>, <<"test.txt">>),
%     ?assertEqual(<<"test">>, MpAttachment1),

%     TestFileName = data_path("1M"),
%     {ok, FileInfo} = file:read_file_info(TestFileName),
%     FileSize = FileInfo#file_info.size,

%     {ok, Doc1} = couchdb:save_doc(Db, {[{<<"_id">>, <<"test5">>}]},
%                                          [{<<"1M">>, {file, TestFileName}}], []),

%     ?assert(couchdb_doc:is_saved(Doc1)),
%     {ok, Doc2} = couchdb:open_doc(Db, <<"test5">>),
%     MpAttachments = couchdb_doc:get_value(<<"_attachments">>, Doc2),
%     ?assert(MpAttachments /= undefined),
%     MpAttachment2 = couchdb_doc:get_value(<<"1M">>, MpAttachments),
%     ?assert(MpAttachment2 /= undefined),
%     ?assertEqual(FileSize, couchdb_doc:get_value(<<"length">>, MpAttachment2)),
%     {ok, MpBin} = couchdb:fetch_attachment(Db, <<"test5">>, <<"1M">>),
%     ?assertEqual(FileSize, iolist_size(MpBin)),
%     {ok, MpDoc2} = couchdb:save_doc(Db, Doc2, [{<<"hello.txt">>, <<"world">>}], []),
%     ?assert(couchdb_doc:is_saved(MpDoc2)),
%     MpAttachments3= couchdb_doc:get_value(<<"_attachments">>, MpDoc2),
%     ?assert(MpAttachments3 /= undefined),
%     MpAttachment4 = couchdb_doc:get_value(<<"1M">>, MpAttachments3),
%     ?assert(MpAttachment4 /= undefined),
%     {ok, MpBin1} = couchdb:fetch_attachment(Db, <<"test5">>, <<"1M">>),
%     ?assertEqual(FileSize, iolist_size(MpBin1)),
%     MpAttachment5 = couchdb_doc:get_value(<<"hello.txt">>, MpAttachments3),
%     ?assert(MpAttachment5 /= undefined),
%     {ok, MpBin2} = couchdb:fetch_attachment(Db, <<"test5">>, <<"hello.txt">>),
%     ?assertEqual(<<"world">>, MpBin2),
%     ok.



% collect_mp({doc, Doc, Next}, Acc) ->
%     collect_mp(couchdb:stream_doc(Next), [{doc, Doc} | Acc]);
% collect_mp({att, Name, Next}, Acc) ->
%     collect_mp(couchdb:stream_doc(Next), [{Name, <<>>} | Acc]);
% collect_mp({att_body, Name, Chunk, Next}, Acc) ->
%     Buffer = proplists:get_value(Name, Acc),
%     NBuffer = << Buffer/binary, Chunk/binary >>,
%     Acc1 = lists:keystore(Name, 1, Acc, {Name, NBuffer}),
%     collect_mp(couchdb:stream_doc(Next), Acc1);
% collect_mp({att_eof, _Name, Next}, Acc) ->
%     collect_mp(couchdb:stream_doc(Next), Acc);
% collect_mp(eof, Acc) ->
%     Acc.

% data_path(Basename) ->
%     FName = filename:join([filename:dirname(filename:absname(?FILE)), "..", "support",
%                            Basename]),

%     case filelib:is_file(FName) of
%         true -> FName;
%         false ->
%             F = filename:join([filename:dirname(code:which(?MODULE)), "..", "support",
%                                Basename]),
%             F
%     end.


% -endif.