-module(couchdb_uuid_tests).

-include ( "../src/dev.hrl" ).
-include_lib("eunit/include/eunit.hrl").

utc_id_time_does_not_advance_test() ->
    % Timestamp didn't advance but local clock sequence should and new UUIds
    % should be generated
    Now = {0, 1, 2},
    ClockSeq0 = couchdb_uuids:micros_since_epoch({3, 4, 5}),
    {UtcId0, ClockSeq1} = couchdb_uuids:utc_suffix("", ClockSeq0, Now),
    ?assert(is_binary(UtcId0)),
    ?assertEqual(ClockSeq0 + 1, ClockSeq1),
    {UtcId1, ClockSeq2} = couchdb_uuids:utc_suffix("", ClockSeq1, Now),
    ?assertNotEqual(UtcId0, UtcId1),
    ?assertEqual(ClockSeq1 + 1, ClockSeq2).


utc_id_time_advanced_test() ->
    % Timestamp advanced, a new UUID generated and also the last clock sequence
    % is updated to that timestamp.
    Now0 = {0, 1, 2},
    ClockSeq0 = couchdb_uuids:micros_since_epoch({3, 4, 5}),
    {UtcId0, ClockSeq1} = couchdb_uuids:utc_suffix("", ClockSeq0, Now0),
    ?assert(is_binary(UtcId0)),
    ?assertEqual(ClockSeq0 + 1, ClockSeq1),
    Now1 = {9, 9, 9},
    {UtcId1, ClockSeq2} = couchdb_uuids:utc_suffix("", ClockSeq1, Now1),
    ?assert(is_binary(UtcId1)),
    ?assertNotEqual(UtcId0, UtcId1),
    ?assertEqual(couchdb_uuids:micros_since_epoch(Now1), ClockSeq2).

utc_random_test_time_does_not_advance_test() ->
    {MSec, Sec, USec} = os:timestamp(),
    Future = {MSec + 10, Sec, USec},
    ClockSeqFuture = couchdb_uuids:micros_since_epoch(Future),
    {UtcRandom, NextClockSeq} = couchdb_uuids:utc_random(ClockSeqFuture),
    ?assert(is_binary(UtcRandom)),
    ?assertEqual(32, byte_size(UtcRandom)),
    ?assertEqual(ClockSeqFuture + 1, NextClockSeq).

utc_random_test_time_advance_test() ->
    ClockSeqPast = couchdb_uuids:micros_since_epoch({1, 1, 1}),
    {UtcRandom, NextClockSeq} = couchdb_uuids:utc_random(ClockSeqPast),
    ?assert(is_binary(UtcRandom)),
    ?assertEqual(32, byte_size(UtcRandom)),
    ?assert(NextClockSeq > couchdb_uuids:micros_since_epoch({1000, 0, 0})).