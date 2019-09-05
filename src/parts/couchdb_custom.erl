-module(couchdb_custom).

-include("couchdb.hrl").

-export([
    get_document_id/1
    ,get_document_rev/1
]).

%% @reference Custom API
%% @doc Returns the Id of a Document
-spec generate_unique_id() -> binary().
generate_unique_id() ->
    generate_uuid_v4().

%% @reference Custom API
%% @doc Returns the Id of a Document
-spec get_document_id(map()) -> binary() | atom().
get_document_id(#{<<"_id">> := <<Id/binary>>}=_Document) -> Id;
get_document_id(_Other) -> undefined.

%% @reference Custom API
%% @doc Returns the Rev of a Document
-spec get_document_rev(map()) -> binary() | atom().
get_document_rev(#{<<"_rev">> := <<Rev/binary>>}=_Document) -> Rev;
get_document_rev(_Other) -> undefined.


%
%   PRIVATE
%
%% @private
generate_uuid_v4() ->
    quickrand:seed ( ),
    list_to_binary ( uuid:uuid_to_string ( uuid:get_v4_urandom ( ) ) ).