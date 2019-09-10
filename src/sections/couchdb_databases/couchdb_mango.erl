-module(couchdb_mango).

-include("couchdb.hrl").
-include("../../dev.hrl").

-export([
    find/2
]).



find(#db{server=#server{url=ServerUrl}, name=DbName, options=Opts}, Req ) -> 
    case check_request_compliance(Req) of
        true -> 
            Headers = [{<<"Content-Type">>, <<"application/json">>}],      
            Url = hackney_url:make_url(ServerUrl, <<DbName/binary,"/_find">>, []),
            ?PRINT(Url),
            Resp = couchdb_httpc:request(post, Url, Headers, couchdb_ejson:encode(Req), Opts),
            case couchdb_httpc:db_resp(Resp, [200]) of
                {ok, _, _, Ref} ->
                    {ok, couchdb_httpc:json_body(Ref)};
                Error -> Error
            end;
        false -> {error, <<"Invalid request">>}
    end.


%% @private
check_request_compliance(#{<<"selector">> := _Selector}=Req) ->
    RequestDefinition = #{
        <<"selector">> => fun(X) -> is_map(X) end,
        <<"limit">> => fun(X) -> is_integer(X) end,
        <<"skip">> => fun(X) -> is_integer(X) end,
        <<"sort">> => fun(X) -> sortfunc, true end,
        <<"fields">> => fun(X) -> is_list(X) end,
        <<"use_index">> => fun(X) -> is_binary(X) orelse is_list(X) end,
        <<"r">> => fun(X) -> is_integer(X) end,
        <<"bookmark">> => fun(X) -> is_binary(X) end,
        <<"update">> => fun(X) -> is_boolean(X) end,
        <<"stable">> => fun(X) -> is_boolean(X) end,
        <<"stale">> => fun(X) -> is_binary(X) end,
        <<"execution_stats">> => fun(X) -> is_boolean(X) end
    },

    maps:fold(fun(Key, Value, Acc)->
        case Acc of 
            true ->
                case maps:get(Key, RequestDefinition) of
                    Check when is_function(Check, 1) -> Check(Value);
                    _Other -> false
                end;
            false -> false
        end  
    end, true, Req);
check_request_compliance(_) -> false.