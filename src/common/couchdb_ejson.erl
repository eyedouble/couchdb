%% @hidden
-module(couchdb_ejson).

-export([encode/1, decode/1]).

-include("couchdb.hrl").
-include("./../dev.hrl").


-define(JSON_ENCODE(D),  json_encode(D)).
-define(JSON_DECODE(D), json_decode(D)).



%% @doc encode an erlang term to JSON. Throw an exception if there is
%% any error.
-spec encode(ejson()) -> binary().
encode(D) ->
    ?JSON_ENCODE(D).


%% @doc decode a binary to an EJSON term. Throw an exception if there is
%% any error.
-spec decode(binary()) -> ejson().
decode(D) ->
    try
        ?JSON_DECODE(D)
    catch
        throw:Error ->
            throw({invalid_json, Error});
        error:badarg ->
            throw({invalid_json, badarg})
    end.

%   COUCHDB IMPLEMENTATION
%
json_encode(V) ->
    jiffy:encode(V, [
        use_nil
        ,force_utf8    
    ]).

json_decode(V) ->
    try
        jiffy:decode(V, [
            use_nil
            ,dedupe_keys 
            ,return_maps            
        ])
    catch
        throw:Error ->
            throw({invalid_json, Error})
    end.

