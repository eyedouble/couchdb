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


% -define(TEST_INIT(),
%     {ok, _} = application:ensure_all_started(couchdb),
%     Server = couchdb:server_record(<<"http://localhost:5984">>),
%     [ catch couchdb_databases:delete(Server, MockDb) || MockDb <- ?MOCK_DBS ],
%     Server
% ).