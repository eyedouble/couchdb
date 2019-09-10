-module(couchdb_attachments_tests).

-include_lib("eunit/include/eunit.hrl").

-include ( "../src/dev.hrl" ).
-include("../../test_helper.hrl").


%
%   HELPER
%
init() ->
    {ok, _} = application:ensure_all_started(couchdb),
    {ok, Server} = couchdb:server_record(<<"http://localhost:5984">>),
    [ catch couchdb_databases:delete(Server, MockDb) || MockDb <- ?MOCK_DBS ],
    Server.

%
%   TESTS
%
put_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    {ok, #{<<"_id">> := _Id, <<"_rev">> := Rev}=Doc} = couchdb_documents:save(Db, ?MOCK_DOCS(1)),
    {ok, #{<<"_id">> := _Id1, <<"_rev">> := Rev1}=_Res1} = couchdb_attachments:put(Db, Doc, <<"Example Attachment">>, <<"Example Body">>),
    ?assertNot(Rev =:= Rev1).

get_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    {ok, #{<<"_id">> := _Id, <<"_rev">> := Rev}=Doc} = couchdb_documents:save(Db, ?MOCK_DOCS(1)),
    {ok, #{<<"_id">> := _Id1, <<"_rev">> := Rev1}=_Res1} = couchdb_attachments:put(Db, Doc, <<"Example Attachment">>, <<"Example Body">>),
    {ok, Res2} = couchdb_attachments:get(Db, Doc, <<"Example Attachment">>),
    ?assertMatch(<<"Example Body">>, Res2).

get_stream_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    {ok, #{<<"_id">> := _Id, <<"_rev">> := _Rev}=Doc} = couchdb_documents:save(Db, ?MOCK_DOCS(1)),
    {ok, #{<<"_id">> := _Id1, <<"_rev">> := _Rev1}=_Res1} = couchdb_attachments:put(Db, Doc, <<"Example Attachment">>, <<"Example Body">>),
    {ok, StreamRef} = couchdb_attachments:get_stream(Db, Doc, <<"Example Attachment">>, #{}),
    ?assert(is_reference(StreamRef)),
    {ok, Res2} = couchdb_attachments:stream(StreamRef), 
    ?assertMatch(<<"Example Body">>, Res2).

delete_test() -> 
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    {ok, #{<<"_id">> := _Id, <<"_rev">> := _Rev}=Doc} = couchdb_documents:save(Db, ?MOCK_DOCS(1)),
    {ok, #{<<"_id">> := _Id1, <<"_rev">> := _Rev1}=Res1} = couchdb_attachments:put(Db, Doc, <<"Example Attachment">>, <<"Example Body">>),
    Doc2 = maps:merge(Doc, Res1),
    {ok, Res2} = couchdb_attachments:delete(Db, Doc2, <<"Example Attachment">>),
    ?assertMatch(#{<<"_id">> := <<_/binary>>, <<"_rev">> := <<"3-", _/binary>>}, Res2).




% RevDoc11 = proplists:get_value(<<"rev">>, Res),
% ?assertNot(RevDoc1 =:= RevDoc11),
% {ok, Attachment} = couchdb:fetch_attachment(Db, "test", "test"),
% ?assertEqual( <<"test">>, Attachment),
% {ok, Doc2} = couchdb:open_doc(Db, "test"),
% ?assertMatch({ok, {_}}, couchdb:delete_attachment(Db, Doc2, "test")),
% Doc3 = {[{<<"_id">>, <<"test2">>}]},
% Doc4 = couchdb_attachments:add_inline(Doc3, "test", "test.txt"),
% Doc5 = couchdb_attachments:add_inline(Doc4, "test2", "test2.txt"),
% {ok, _} = couchdb:save_doc(Db, Doc5),
% {ok, Attachment1} = couchdb:fetch_attachment(Db, "test2", "test.txt"),
% {ok, Attachment2} = couchdb:fetch_attachment(Db, "test2", "test2.txt"),
% ?assertEqual( <<"test">>, Attachment1),
% ?assertEqual( <<"test2">>, Attachment2),
% {ok, Doc6} = couchdb:open_doc(Db, "test2"),
% Doc7 = couchdb_attachments:delete_inline(Doc6, "test2.txt"),
% {ok, _} = couchdb:save_doc(Db, Doc7),
% ?assertEqual({error, not_found}, couchdb:fetch_attachment(Db, "test2", "test2.txt")),
% {ok, Attachment4} = couchdb:fetch_attachment(Db, "test2", "test.txt"),
% ?assertEqual( <<"test">>, Attachment4),
% {ok, Doc8} = couchdb:save_doc(Db, {[]}),

% TestFileName = data_path("1M"),
% {ok, FileInfo} = file:read_file_info(TestFileName),
% {ok, Fd} = file:open(TestFileName, [read]),
% FileSize = FileInfo#file_info.size,
% StreamFun = fun() ->
%                 case file:read(Fd, 4096) of
%                     {ok, Data} ->  {ok, iolist_to_binary(Data)};
%                     _ -> eof
%                 end
%             end,
% {ok, _Res2} = couchdb:put_attachment(Db, couchdb_doc:get_id(Doc8), "1M", StreamFun,
%                                       [{content_length, FileSize}, {rev, couchdb_doc:get_rev(Doc8)}]),

% file:close(Fd),
% {ok, Doc9} = couchdb:open_doc(Db, couchdb_doc:get_id(Doc8)),
% Attachements = couchdb_doc:get_value(<<"_attachments">>, Doc9),
% ?assert(Attachements /= undefined),
% Attachment5 = couchdb_doc:get_value(<<"1M">>, Attachements),
% ?assert(Attachment5 /= undefined),
% ?assertEqual(FileSize, couchdb_doc:get_value(<<"length">>, Attachment5)),
% {ok, Bin} = couchdb:fetch_attachment(Db, couchdb_doc:get_id(Doc8), "1M"),
% ?assertEqual(FileSize, iolist_size(Bin)),

% {ok, _Res3}= couchdb:put_attachment(Db, "test/2", "test", "test"),
% {ok, Attachment10} = couchdb:fetch_attachment(Db, "test/2", "test"),
% ?assertEqual(<<"test">>, Attachment10),
% {ok, _Res4}= couchdb:put_attachment(Db, "test3", "test", "test"),
% {ok, Attachment11} = couchdb:fetch_attachment(Db, "test3", "test"),
% ?assertEqual(<<"test">>, Attachment11),
% ok.


%%
%%   MULTIPART
%%
%%  Irsan:
%%  This is left here for reference.
%%  The handling of multipart/streaming etc should be completely 
%%  reconsidered probably during a rewrit using 99/GUN http client.
%%

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