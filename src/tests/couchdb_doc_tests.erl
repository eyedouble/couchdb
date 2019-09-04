-module(couchdb_doc_tests).

-include ( "../src/dev.hrl" ).
-include_lib("eunit/include/eunit.hrl").

% -ifdef(TEST).

% get_value_test() ->
%     Doc = {[{<<"a">>, 1}]},
%     ?assertEqual(1, couchdb_doc:get_value(<<"a">>, Doc)),
%     ?assertEqual(1, couchdb_doc:get_value("a", Doc)),
%     ?assertEqual(undefined, couchdb_doc:get_value("b", Doc)),
%     ?assertEqual(nil, couchdb_doc:get_value("b", Doc, nil)),
%     ok.

% set_value_test() ->
%     Doc = {[{<<"a">>, 1}]},
%     ?assertEqual(undefined, couchdb_doc:get_value("b", Doc)),
%     Doc1 = couchdb_doc:set_value("b", 1, Doc),
%     ?assertEqual(1, couchdb_doc:get_value("b", Doc1)),
%     Doc2 = couchdb_doc:set_value("b", 0, Doc1),
%     ?assertEqual(0, couchdb_doc:get_value("b", Doc2)),
%     ok.

% delete_value_test() ->
%     Doc = {[{<<"a">>, 1}, {<<"b">>, 1}]},
%     Doc1 = couchdb_doc:delete_value("b", Doc),
%     ?assertEqual(undefined, couchdb_doc:get_value("b", Doc1)),
%     ok.

% extend_test() ->
%     Doc = {[{<<"a">>, 1}]},
%     ?assertEqual(1, couchdb_doc:get_value("a", Doc)),
%     ?assertEqual(undefined, couchdb_doc:get_value("b", Doc)),
%     ?assertEqual(undefined, couchdb_doc:get_value("c", Doc)),
%     Doc1 = couchdb_doc:extend([{<<"b">>, 1}, {<<"c">>, 1}], Doc),
%     ?assertEqual(1, couchdb_doc:get_value("b", Doc1)),
%     ?assertEqual(1, couchdb_doc:get_value("c", Doc1)),
%     Doc2 = couchdb_doc:extend([{<<"b">>, 3}, {<<"d">>, 1}], Doc1),
%     ?assertEqual(3, couchdb_doc:get_value("b", Doc2)),
%     ?assertEqual(1, couchdb_doc:get_value("d", Doc2)),
%     ok.

% id_rev_test() ->
%     Doc = {[{<<"a">>, 1}]},
%     ?assertEqual(undefined, couchdb_doc:get_id(Doc)),
%     ?assertEqual(undefined, couchdb_doc:get_rev(Doc)),
%     ?assertEqual({undefined, undefined}, couchdb_doc:get_idrev(Doc)),
%     Doc1 = couchdb_doc:extend([{<<"_id">>, 1}, {<<"_rev">>, 1}], Doc),
%     ?assertEqual(1, couchdb_doc:get_id(Doc1)),
%     ?assertEqual(1, couchdb_doc:get_rev(Doc1)),
%     ?assertEqual({1, 1}, couchdb_doc:get_idrev(Doc1)),
%     ok.

% is_saved_test() ->
%     Doc = {[{<<"a">>, 1}]},
%     ?assertEqual(false, couchdb_doc:is_saved(Doc)),
%     Doc1 = couchdb_doc:set_value(<<"_rev">>, <<"x">>, Doc),
%     ?assertEqual(true, couchdb_doc:is_saved(Doc1)),
%     ok.


% take_value_test() ->
%     Doc = {[{<<"a">>, 1}, {<<"b">>, 2}]},
%     ?assertEqual({undefined, Doc}, couchdb_doc:take_value(<<"c">>, Doc)),
%     ?assertEqual({1, {[{<<"b">>, 2}]}}, couchdb_doc:take_value(<<"a">>, Doc)),
%     ok.

% -endif.