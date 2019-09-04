-define(MOCK_DBS, [
    <<"test_db0">>, <<"test_db1">>, <<"test_db2">>
]).

-define(MOCK_DBS(N), lists:nth(N, ?MOCK_DBS)).

-define(MOCK_DOCS, [
    #{
        <<"title">> => <<"Untitled 1">>,
        <<"content">> => <<"Example Content">>
    },
    #{
        <<"title">> => <<"Untitled 2">>,
        <<"content">> => <<"Example Content">>
    },
    #{
        <<"title">> => <<"Untitled 3">>,
        <<"content">> => <<"Example Content">>
    }
]).

-define(MOCK_DOCS(N), lists:nth(N, ?MOCK_DOCS)).


-define(TEST_INIT(),
    % Start Deps
    {ok, _} = application:ensure_all_started(couchdb),

    % clean_dbs() ->
    Server = couchdb:server_connection(),
    [ catch couchdb:delete_db(Server, MockDb) || MockDb <- ?MOCK_DBS ],
    timer:sleep(300),

    % init() ->
    couchdb:server_connection(<<"http://localhost:5984">>)
).