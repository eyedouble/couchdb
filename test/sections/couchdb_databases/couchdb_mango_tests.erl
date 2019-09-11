-module(couchdb_mango_tests).

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
find_nothing_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    {ok, Res1} = couchdb_mango:find(Db, #{
        <<"selector">> => #{
            <<"firstname">> => <<"Bla">>
        }
    }),
    ?assertMatch(#{
        <<"bookmark">> := _,
        <<"docs">> := [],
        <<"warning">> := _}, Res1).

find_single_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    {ok, #{<<"_id">> := _Id1}=_Doc1} = couchdb_documents:save(Db, ?MOCK_DOCS(1)),
    {ok, #{<<"_id">> := _Id2}=_Doc2} = couchdb_documents:save(Db, ?MOCK_DOCS(2)),
    {ok, #{<<"_id">> := _Id3}=Doc3} = couchdb_documents:save(Db, ?MOCK_DOCS(3)),
    {ok, Res1} = couchdb_mango:find(Db, #{
        <<"selector">> => #{
            <<"title">> => <<"Untitled 3">>
        }
    }),

    ?assertMatch(#{
        <<"bookmark">> := _,
        <<"docs">> := [Doc3],
        <<"warning">> := _}, Res1).

find_multiple_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    {ok, #{<<"_id">> := _Id1}=Doc1} = couchdb_documents:save(Db, ?MOCK_DOCS(1)),
    {ok, #{<<"_id">> := _Id2}=Doc2} = couchdb_documents:save(Db, ?MOCK_DOCS(2)),
    {ok, #{<<"_id">> := _Id3}=Doc3} = couchdb_documents:save(Db, ?MOCK_DOCS(3)),
    ?PRINT(Doc3),
    {ok, Res1} = couchdb_mango:find(Db, #{
        <<"selector">> => #{
            <<"content">> => <<"Example Content">>
        }
    }),
    ?assertMatch(#{
        <<"bookmark">> := _,
        <<"docs">> := [_, _, _],
        <<"warning">> := _}, Res1).

find_nested_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    {ok, #{<<"_id">> := _Id1}=_Doc1} = couchdb_documents:save(Db, ?MOCK_DOCS(1)),
    {ok, #{<<"_id">> := _Id2}=_Doc2} = couchdb_documents:save(Db, ?MOCK_DOCS(2)),
    {ok, #{<<"_id">> := _Id3}=Doc3} = couchdb_documents:save(Db, #{
        <<"_id">> => <<"fakeid1">>,
        <<"object">> => #{
            <<"object2">> => #{
                <<"colour">> => <<"pink">>
            }
        }
    }),
    {ok, Res1} = couchdb_mango:find(Db, #{
        <<"selector">> => #{            
            <<"object">> => #{
                    <<"object2">> => #{
                    <<"colour">> => <<"pink">>
                }
            }
        }
    }),

    ?assertMatch(#{
        <<"bookmark">> := _,
        <<"docs">> := [Doc3],
        <<"warning">> := _}, Res1).

find_nested_shorthand_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    {ok, #{<<"_id">> := _Id1}=_Doc1} = couchdb_documents:save(Db, ?MOCK_DOCS(1)),
    {ok, #{<<"_id">> := _Id2}=_Doc2} = couchdb_documents:save(Db, ?MOCK_DOCS(2)),
    {ok, #{<<"_id">> := _Id3}=Doc3} = couchdb_documents:save(Db, #{
        <<"_id">> => <<"fakeid1">>,
        <<"object">> => #{
            <<"object2">> => #{
                <<"colour">> => <<"pink">>
            }
        }
    }),
    {ok, Res1} = couchdb_mango:find(Db, #{
        <<"selector">> => #{            
            <<"object.object2.colour">> => <<"pink">>
        }
    }),

    ?assertMatch(#{
        <<"bookmark">> := _,
        <<"docs">> := [Doc3],
        <<"warning">> := _}, Res1).


index_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    {ok, Res1} = couchdb_mango:index(Db, #{
        <<"index">> => #{
            <<"fields">> => [<<"object.object2.colour">>]
        },
        <<"name">> => <<"test-index">>
    }),

    ?assertMatch(#{
        <<"id">> := _,
        <<"name">> := <<"test-index">>,
        <<"result">> := _}, Res1).


index_filter_test() ->
    Server = init(),
    {ok, Db} = couchdb_databases:create(Server, ?MOCK_DBS(1)),
    {ok, Res0} = couchdb_mango:index(Db, #{
        <<"index">> => #{
            <<"fields">> => [<<"object.object2.colour">>]
        },
        <<"name">> => <<"test-index">>
    }),

    ?assertMatch(#{
        <<"id">> := _,
        <<"name">> := <<"test-index">>,
        <<"result">> := _}, Res0),
    
    {ok, #{<<"_id">> := _Id1}=_Doc1} = couchdb_documents:save(Db, ?MOCK_DOCS(1)),
    {ok, #{<<"_id">> := _Id2}=_Doc2} = couchdb_documents:save(Db, ?MOCK_DOCS(2)),
    {ok, #{<<"_id">> := _Id3}=Doc3} = couchdb_documents:save(Db, #{
        <<"_id">> => <<"fakeid1">>,
        <<"object">> => #{
            <<"object2">> => #{
                <<"colour">> => <<"pink">>
            }
        }
    }),
    {ok, Res1} = couchdb_mango:find(Db, #{
        <<"selector">> => #{            
            <<"object.object2.colour">> => <<"pink">>
        }
    }),

    ?assertNotMatch(#{<<"warning">> := _}, Res1),
    ?assertMatch(#{
        <<"bookmark">> := _,
        <<"docs">> := [Doc3]}, Res1).





