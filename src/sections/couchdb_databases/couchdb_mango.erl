-module(couchdb_mango).

-include("couchdb.hrl").
-include("../../dev.hrl").

-export([
    find/2
    ,index/2
]).



%% @doc Create a new index on a database
%%
%%      ```
%%        Request = #{
%%              <<"selector">> => map(), % required
%%              <<"limit">> => integer(),
%%              <<"skip">> => integer(),
%%              <<"sort">> => map(), % CouchDB Sort Syntax
%%              <<"fields">> => list(),
%%              <<"use_index">> => binary() | list(),
%%              <<"r">> => integer(),
%%              <<"bookmark">> => binary(), 
%%              <<"update">> => boolean(),
%%              <<"stable">> => boolean(),
%%              <<"stale">> => binary(),
%%              <<"execution_stats">> => boolean(),
%%        }
%%      '''
-spec(find(Database::db(), Request::map()) -> {ok, map()} | {error, binary()}).
find(#db{server=#server{url=ServerUrl}, name=DbName, options=Opts}, Req) -> 
    case check_find_request_compliance(Req) of
        true -> 
            Headers = [{<<"Content-Type">>, <<"application/json">>}],      
            Url = hackney_url:make_url(ServerUrl, <<DbName/binary,"/_find">>, []),
            Resp = couchdb_httpc:request(post, Url, Headers, couchdb_ejson:encode(Req), Opts),
            case couchdb_httpc:db_resp(Resp, [200]) of
                {ok, _, _, Ref} ->
                    {ok, couchdb_httpc:json_body(Ref)};
                Error -> Error
            end;
        false -> {error, <<"Invalid request">>}
    end.

%% @doc Create a new index on a database
%%
%%      ```
%%        Request = #{
%%            <<"index">> => map(), % required
%%            <<"ddoc">> => binary(),
%%            <<"name">> => binary(),
%%            <<"type">> => binary(),
%%            <<"partial_filter_selector">> => map()
%%        }
%%      '''
-spec(index(Database::db(), Request::map()) -> {ok, map()} | {error, binary()}).
index(#db{server=#server{url=ServerUrl}, name=DbName, options=Opts}, Req) ->
    case check_index_request_compliance(Req) of
        true -> 
            Headers = [{<<"Content-Type">>, <<"application/json">>}],      
            Url = hackney_url:make_url(ServerUrl, <<DbName/binary,"/_index">>, []), 
            Resp = couchdb_httpc:request(post, Url, Headers, couchdb_ejson:encode(Req), Opts),
            case couchdb_httpc:db_resp(Resp, [200]) of
                {ok, _, _, Ref} ->
                    {ok, couchdb_httpc:json_body(Ref)};
                Error -> Error
            end;
        false -> {error, <<"Invalid request">>}
    end.

%% @doc Get a list of all indexes in the database.
-spec(get_indexes(Database::db()) -> {ok, list()} | {error, binary()}).
get_indexes(Db) -> nyi.


%
%   Internal
%
%% @private
check_find_request_compliance(#{<<"selector">> := _Selector}=Req) ->
    check_request_compliance(#{
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
    }, Req);
%% @private
check_find_request_compliance(_) -> false.

%% @private
check_index_request_compliance(#{<<"index">> := _Selector}=Req) ->
    check_request_compliance(#{
        <<"index">> => fun(X) -> is_map(X) end,
        <<"ddoc">> => fun(X) -> is_binary(X) end,
        <<"name">> => fun(X) -> is_binary(X) end,
        <<"type">> => fun(X) -> X =:= <<"json">> orelse X =:= <<"text">> orelse X =:= <<"geo">> end,
        <<"partial_filter_selector">> => fun(X) -> is_map(X) end
    }, Req);
%% @private
check_index_request_compliance(_) -> false.

%% @private
check_request_compliance(RequestDefinition, Request) ->
     maps:fold(fun(Key, Value, Acc)->
        case Acc of 
            true ->
                case maps:get(Key, RequestDefinition) of
                    Check when is_function(Check, 1) -> Check(Value);
                    _Other -> false
                end;
            false -> false
        end  
    end, true, Request).