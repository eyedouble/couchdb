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
    D = couchdb_ejson:encode(#{<<"unicode">> => <<"Noé Böllinger"/utf8>>}),
    ?assertEqual(<<"{\"unicode\":\"Noé Böllinger\"}"/utf8>>, D).

special_char2_test() ->
    D = couchdb_ejson:encode(#{<<"unicode">> => <<"Noé Böllinger"/utf8>>}),
    D1 = couchdb_ejson:decode(D),    
    ?assertEqual(#{<<"unicode">> => <<"Noé Böllinger"/utf8>>}, D1).

special_char3_test() ->
    D = couchdb_ejson:encode(#{<<"unicode">> => <<"Noé"/utf8>>}),
    ?assertEqual(<<"{\"unicode\":\"Noé\"}"/utf8>>, D).

special_char4_test() ->
    D = couchdb_ejson:encode(#{<<"unicode">> => <<"Noé"/utf8>>}),
    D1 = couchdb_ejson:decode(D),    
    ?assertEqual(#{<<"unicode">> => <<"Noé"/utf8>>}, D1).
