-module(couchdb_common_urls).

-include("couchdb.hrl").
-include("../dev.hrl").

% -export([
%    url/4
% ]).

% -define(COMMON_REQ_HEADERS, #{<<"Accept">> => <<"application/json">>}).
% -define(COMMON_RES_HEADERS, #{<<"Content-Type">> => <<"application/json">>}).

% -record(endpoint_req, {headers=?COMMON_REQ_HEADERS, queries=#{}}).
% -record(endpoint_res, {headers=?COMMON_RES_HEADERS, codes=[200]}).
% -record(endpoint, {res=#endpoint_res{}, req=#endpoint_req{}}).



% url(<<"/">>=Url, get, Headers, Queries) ->
%     case compliant(#endpoint{}, Headers, Queries) of
%         true -> common_request()

%     common_endpoint().




% %
% %   Internal
% %

% common_endpoint(#endpoint{}=Req, #{}=Headers, #{}=Queries) ->
%     case headers_are_compliant(Req, Headers)
%          andalso queries_are_compliant(Req, Queries) of
%         true -> 
%         false -> {error, <<"Not compliant">>}
%     end.

% headers_are_compliant(#endpoint{req=#endpoint_req{headers=AllowedHeaders}}=_Endpoint, Headers) ->
%     case maps:fold(fun(Key, _Val, Acc) -> 
%         maps:remove(Key,Acc)
%     end, Headers, AllowedHeaders) of
%        #{} -> true;
%        _Declined -> false
%     end.

% queries_are_compliant(#endpoint{req=#endpoint_req{queries=AllowedQueries}}=_Endpoint, Queries) ->
%     case maps:fold(fun(Key, _Val, Acc) -> 
%         maps:remove(Key,Acc)
%     end, Queries, AllowedQueries) of
%        #{} -> true;
%        _Declined -> false
%     end.
