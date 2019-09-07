-module(couchdb_documents_tests).

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
exists_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    ?assertMatch(false, couchdb_documents:exists(Db, <<"fakeid">>)),

    {ok, #{<<"_id">> := Id}=_Doc} = couchdb_documents:save(Db, ?MOCK_DOCS(1)),
    ?assertMatch(true, couchdb_documents:exists(Db, Id)).

lookup_rev_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    {ok, #{<<"_id">> := Id, <<"_rev">> := Rev0}=_Doc} = couchdb_documents:save(Db, ?MOCK_DOCS(1)),
    Rev1 = couchdb_documents:lookup_rev(Db, Id),
    ?assertEqual(Rev1, Rev0).

get_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    ?assertEqual({error, not_found}, couchdb_documents:get(Db, <<"test2">>)),

    {ok, #{<<"_id">> := Id}=_Doc} = couchdb_documents:save(Db, ?MOCK_DOCS(1)),
    ?assertMatch({ok, #{
        <<"_id">> := Id,
        <<"_rev">> := <<"1-", _Rev/binary>>
    }}, couchdb_documents:get(Db, Id)).
 

save_without_id_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    Doc = couchdb_documents:save(Db, ?MOCK_DOCS(1)),

    ?assertMatch({ok, #{
        <<"content">> := <<"Example Content">>,
        <<"_id">> := <<_/binary>>,
        <<"_rev">> := <<"1-", _/binary>>,
        <<"title">> := <<"Untitled 1">>
    }}, Doc).

save_with_id_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    Doc0 = ?MOCK_DOCS(1),
    Doc1 = maps:put(<<"_id">>, <<"randomid">>, Doc0),
    Doc = couchdb_documents:save(Db, Doc1),

    ?assertMatch({ok, #{
        <<"content">> := <<"Example Content">>,
        <<"_id">> := <<"randomid">>,
        <<"_rev">> := <<"1-", _/binary>>,
        <<"title">> := <<"Untitled 1">>
    }}, Doc).

save_update_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    Doc0 = ?MOCK_DOCS(1),
    Doc1 = maps:put(<<"_id">>, <<"randomid">>, Doc0),
    {ok, Doc2} = couchdb_documents:save(Db, Doc1), 
    Doc3 = maps:put(<<"content">>, <<"Updated Example Content">>, Doc2),
    Doc4 = couchdb_documents:save(Db, Doc3),

    ?assertMatch({ok, #{
        <<"content">> := <<"Updated Example Content">>,
        <<"_id">> := <<"randomid">>,
        <<"_rev">> := <<"2-", _/binary>>,
        <<"title">> := <<"Untitled 1">>
    }}, Doc4).


delete_test() -> 
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    {ok, Doc} = couchdb_documents:save(Db, ?MOCK_DOCS(1)),

    ?assertMatch({ok, [#{
        <<"_id">> := <<_/binary>>,
        <<"_deleted">> := true,
        <<"_rev">> := <<"2-", _/binary>>
    }]}, couchdb_documents:delete(Db, Doc)).

