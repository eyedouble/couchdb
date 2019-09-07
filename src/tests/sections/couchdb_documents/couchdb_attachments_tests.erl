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
attachments_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    {ok, #{<<"_id">> := _Id, <<"_rev">> := Rev}=Doc} = couchdb_documents:save(Db, ?MOCK_DOCS(1)),
    {ok, #{<<"_id">> := _Id1, <<"_rev">> := Rev1}=_Res1} = couchdb_attachments:put(Db, Doc, <<"Example Attachment">>, <<"Example Body">>),
    ?assertNot(Rev =:= Rev1).

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