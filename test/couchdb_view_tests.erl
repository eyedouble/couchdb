-module(couchdb_view_tests).

-include ( "../src/dev.hrl" ).
-include_lib("eunit/include/eunit.hrl").

% -ifdef(TEST).
% -include_lib("kernel/include/file.hrl").


% clean_dbs() ->
%     Server = couchdb:server_connection(),
%     catch couchdb:delete_db(Server, "couchdb_testdb"),
%     ok.

% start_couchdb_tests() ->
%     {ok, _} = application:ensure_all_started(couchdb),
%     clean_dbs().


% basic_test() ->
%     start_couchdb_tests(),
%     Server = couchdb:server_connection(),

%     {ok, Db} = couchdb:create_db(Server, "couchdb_testdb"),

%     DesignDoc = {[
%         {<<"_id">>, <<"_design/couchdb">>},
%         {<<"language">>,<<"javascript">>},
%         {<<"views">>,
%             {[{<<"test">>,
%                 {[{<<"map">>,
%                     <<"function (doc) {\n if (doc.type == \"test\") {\n emit(doc._id, doc);\n}\n}">>
%                 }]}
%             },{<<"test2">>,
%                 {[{<<"map">>,
%                     <<"function (doc) {\n if (doc.type == \"test2\") {\n emit(doc._id, null);\n}\n}">>
%                 }]}
%             }]}
%         }
%     ]},

%     Doc = {[
%         {<<"type">>, <<"test">>}
%     ]},

%     couchdb:save_docs(Db, [DesignDoc, Doc, Doc]),
%     couchdb:ensure_full_commit(Db),

%     {ok, AllDocs} = couchdb_view:fetch(Db),
%     ?assertEqual(3, length(AllDocs)),

%     {ok, Rst2} = couchdb_view:fetch(Db, {"couchdb", "test"}),
%     ?assertEqual(2, length(Rst2)),

%     Count = couchdb_view:count(Db, {"couchdb", "test"}),
%     ?assertEqual(2, Count),

%     {ok, {FirstRow}} = couchdb_view:first(Db, {"couchdb", "test"},  [include_docs]),
%     {Doc1} = proplists:get_value(<<"doc">>, FirstRow),
%     ?assertEqual(<<"test">>, proplists:get_value(<<"type">>, Doc1)),

%     Docs = [
%         {[{<<"_id">>, <<"test1">>}, {<<"type">>, <<"test">>}, {<<"value">>, 1}]},
%         {[{<<"_id">>, <<"test2">>}, {<<"type">>, <<"test">>}, {<<"value">>, 2}]},
%         {[{<<"_id">>, <<"test3">>}, {<<"type">>, <<"test">>}, {<<"value">>, 3}]},
%         {[{<<"_id">>, <<"test4">>}, {<<"type">>, <<"test">>}, {<<"value">>, 4}]}
%     ],

%     couchdb:save_docs(Db, Docs),
%     couchdb:ensure_full_commit(Db),

%     {ok, Rst3} = couchdb_view:fetch(Db, {"couchdb", "test"}, [{start_key, <<"test">>}]),
%     ?assertEqual(4, length(Rst3)),

%     {ok, Rst4} = couchdb_view:fetch(Db, {"couchdb", "test"}, [{start_key, <<"test">>}, {end_key, <<"test3">>}]),
%     ?assertEqual(3, length(Rst4)),

%     AccFun = fun(Row, Acc) -> [Row | Acc] end,
%     Rst5 = couchdb_view:fold(AccFun, [], Db, {"couchdb", "test"}, [{start_key, <<"test">>},   {end_key,<<"test3">>}]),
%     ?assertEqual(3, length(Rst5)).

% view_notfound_test() ->
%     start_couchdb_tests(),
%     Server = couchdb:server_connection(),

%     {ok, Db} = couchdb:create_db(Server, "couchdb_testdb"),

%     {error, not_found} = couchdb_view:fetch(Db, {"couchdb", "test"}, []),
%     ok.


% -endif.
