-module(couchdb_databases_tests).

-include_lib("eunit/include/eunit.hrl").

-include ("../src/dev.hrl" ).
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
    Res0 = couchdb_databases:exists(Server, ?MOCK_DBS(1)),
    ?assertEqual(false, Res0),
    couchdb_databases:create(Server, ?MOCK_DBS(1)),
    Res1 = couchdb_databases:exists(Server, ?MOCK_DBS(1)),
    ?assertEqual(true, Res1),
    {ok, Database} = couchdb:database_record(Server, ?MOCK_DBS(1)),
    Res2 = couchdb_databases:exists(Database),
    ?assertEqual(true, Res2).
    

create_test() -> 
    Server = init(),
    {ok, Database} = couchdb:database_record(Server, ?MOCK_DBS(1)),
    {ok, Database2} = couchdb:database_record(Server, ?MOCK_DBS(2)),
    ?assertMatch({ok, _}, couchdb_databases:create(Database)),
    ?assertEqual({error, db_exists}, couchdb_databases:create(Database)),
    ?assertMatch({ok, _}, couchdb_databases:create(Database2)).

delete_test() ->
    Server = init(),
    {ok, Database} = couchdb:database_record(Server, ?MOCK_DBS(1)),
    {ok, Database} = couchdb_databases:create(Database),    
    Res = couchdb_databases:delete(Database),
    ?assertMatch({ok,#{<<"ok">> := true}}, Res).


all_docs_test() -> 
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    Doc0 = ?MOCK_DOCS(1),
    Doc1 = maps:put(<<"_id">>, <<"randomid">>, Doc0),
    couchdb_documents:save(Db, Doc1), 
    Doc2 = maps:put(<<"_id">>, <<"randomid2">>, Doc0),
    couchdb_documents:save(Db, Doc2), 
   

    AllDocs0 = couchdb_databases:all_docs(Db, #{}),
    ?assertMatch({ok,#{<<"total_rows">> := 2}}, AllDocs0),

    AllDocs1 = couchdb_databases:all_docs(Db, #{"include_docs" => true}),
    ?assertMatch({ok, #{<<"rows">> := [#{<<"doc">> := _}|_]}}, AllDocs1).
%
%   Bulk Docs
%
bulk_doc_save_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    Docs = [?MOCK_DOCS(1), ?MOCK_DOCS(2), ?MOCK_DOCS(3)],
    {ok, DocsRes} = couchdb_databases:bulk_docs_save(Db, Docs),

    ?assertMatch([
        #{
            <<"_id">> := <<_/binary>>,
            <<"content">> := <<"Example Content">>,
            <<"title">> := <<"Untitled 1">>,
            <<"_rev">> := <<"1-",_/binary>>
        },
        #{
            <<"_id">> := <<_/binary>>,
            <<"content">> := <<"Example Content">>,
            <<"title">> := <<"Untitled 2">>,
            <<"_rev">> := <<"1-",_/binary>>
        },
        #{
            <<"_id">> := <<_/binary>>,
            <<"content">> := <<"Example Content">>,
            <<"title">> := <<"Untitled 3">>,
            <<"_rev">> := <<"1-",_/binary>>
        }
    ], DocsRes).


bulk_doc_save_update_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    Docs = [?MOCK_DOCS(1), ?MOCK_DOCS(2), ?MOCK_DOCS(3)],
    {ok, DocsRes} = couchdb_databases:bulk_docs_save(Db, Docs),

    ?assertMatch([
        #{
            <<"_id">> := <<_/binary>>,
            <<"content">> := <<"Example Content">>,
            <<"title">> := <<"Untitled 1">>,
            <<"_rev">> := <<"1-",_/binary>>
        },
        #{
            <<"_id">> := <<_/binary>>,
            <<"content">> := <<"Example Content">>,
            <<"title">> := <<"Untitled 2">>,
            <<"_rev">> := <<"1-",_/binary>>
        },
        #{
            <<"_id">> := <<_/binary>>,
            <<"content">> := <<"Example Content">>,
            <<"title">> := <<"Untitled 3">>,
            <<"_rev">> := <<"1-",_/binary>>
        }
    ], DocsRes),

    {ok, DocsRes2} = couchdb_databases:bulk_docs_save(Db, DocsRes),

    ?assertMatch([
        #{
            <<"_id">> := <<_/binary>>,
            <<"content">> := <<"Example Content">>,
            <<"title">> := <<"Untitled 1">>,
            <<"_rev">> := <<"2-",_/binary>>
        },
        #{
            <<"_id">> := <<_/binary>>,
            <<"content">> := <<"Example Content">>,
            <<"title">> := <<"Untitled 2">>,
            <<"_rev">> := <<"2-",_/binary>>
        },
        #{
            <<"_id">> := <<_/binary>>,
            <<"content">> := <<"Example Content">>,
            <<"title">> := <<"Untitled 3">>,
            <<"_rev">> := <<"2-",_/binary>>
        }
    ], DocsRes2).