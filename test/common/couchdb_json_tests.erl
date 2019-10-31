-module(couchdb_json_tests).

-include ( "../src/dev.hrl" ).
-include_lib("eunit/include/eunit.hrl").



nil_to_null_test() ->
    D = couchdb_ejson:decode(<<"{\"test\": null}">>),
    ?assertEqual(#{<<"test">> => nil}, D).

null_to_nil_test() ->
    D = couchdb_ejson:encode(#{<<"test">> => nil}),
    ?assertEqual(<<"{\"test\":null}">>, D).


special_char1_test() ->
    D = couchdb_ejson:encode(#{<<"zz">> => <<"ÜSCHER">>}),
    ?assertEqual(<<"{\"zz\":\"\\u00dcSCHER\"}">>, D).

special_char2_test() ->
    D = couchdb_ejson:encode(#{<<"zz">> => <<"ÜSCHER">>}),
    D1 = couchdb_ejson:decode(D),
    ?assertEqual(#{<<"zz">> => <<"ÜSCHER"/utf8>>}, D1).