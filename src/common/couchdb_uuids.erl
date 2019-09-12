% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.
-module(couchdb_uuids).
% -include_lib("couch/include/couch_db.hrl").

-behaviour(gen_server).
-vsn(3).
% -behaviour(config_listener).
-include("../dev.hrl").

-export([
    new/0 
    ,set_algorithm/1
    ,random/0
    ,start/0
    ,stop/0
    ,init/1
    ,terminate/2
    ,code_change/3
    ,handle_call/3
    ,handle_cast/2
    ,handle_info/2
]).

% config_listener api
% -export([handle_config_change/5, handle_config_terminate/3]).

-define(b2l(V), binary_to_list(V)).
-define(l2b(V), list_to_binary(V)).

%
%   API
%

%% @doc Generate a new uuid
%%
%% At start time, default the algorithm will be set to `random'
%% should you require a different algorithm use the function
%% `set_algorithm/1' first.
-spec(new() -> binary()).
new() -> gen_server:call(?MODULE, create).


%% @doc Set the algorithm used for uuid genration
%%
%% Available algorithms:
%%
%% random: 128 bits of random awesome.
%%
%% sequential: Monotonically increasing ids with random increments. The first 26 hex characters are random, the last 6 increment in random amounts until an overflow occurs. On overflow, the random prefix is regenerated and the process starts over.
%%
%% utc_random: The time since Jan 1, 1970 UTC, in microseconds. The first 14 characters are the time in hex. The last 18 are random.
%%
%% utc_id: The time since Jan 1, 1970 UTC, in microseconds, plus the utc_id_suffix string. The first 14 characters are the time in hex. The uuids/utc_id_suffix string value is appended to these.
%%
%% Usage:
%%  ```
%%      set_algorithm(random).
%%      set_algorithm(utc_random).
%%      set_algorithm(sequential).
%%      set_algorithm({utc_id, <<"suffix">>).
%%  '''
-spec(set_algorithm(Algorithm::term())-> {ok, term() | {error, term()}}).
set_algorithm(Algo) ->
    gen_server:call(?MODULE, {algorithm_change, Algo}).

% @private
random() -> list_to_binary(to_hex(crypto:strong_rand_bytes(16))).
%
%   Internal
%
% @private
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @private
stop() ->
    gen_server:cast(?MODULE, stop).

% @private
init([]) -> {ok, state()}.

% @private
terminate(_Reason, _State) ->
    ok.

% @private
handle_call({algorithm_change, Algo}, _From, _State) ->
    {reply, {ok, Algo}, state(Algo)};
handle_call(create, _From, random) ->
    {reply, random(), random};
handle_call(create, _From, {utc_random, ClockSeq}) ->
    {UtcRandom, NewClockSeq} = utc_random(ClockSeq),
    {reply, UtcRandom, {utc_random, NewClockSeq}};
handle_call(create, _From, {utc_id, UtcIdSuffix, ClockSeq}) ->
    Now = os:timestamp(),
    {UtcId, NewClockSeq} = utc_suffix(UtcIdSuffix, ClockSeq, Now),
    {reply, UtcId, {utc_id, UtcIdSuffix, NewClockSeq}};
handle_call(create, _From, {sequential, Pref, Seq}) ->
    Result = ?l2b(Pref ++ io_lib:format("~6.16.0b", [Seq])),
    case Seq >= 16#fff000 of
        true ->
            {reply, Result, {sequential, new_prefix(), inc()}};
        _ ->
            {reply, Result, {sequential, Pref, Seq + inc()}}
    end.

% @private
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

% @private
handle_info(_Info, State) ->
    {noreply, State}.

% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% @private
new_prefix() -> to_hex((crypto:strong_rand_bytes(13))).

% @private
inc() -> rand:uniform(16#ffd).

% @private
state() -> state(random).
state(Uuid_algorithm) ->
    % AlgoStr = config:get("uuids", "algorithm", "random"),
    case Uuid_algorithm of
        random -> random;
        utc_random ->
            ClockSeq = micros_since_epoch(os:timestamp()),
            {utc_random, ClockSeq};        
        sequential -> {sequential, new_prefix(), inc()};
        {utc_id, Suffix} when is_binary(Suffix) ->
            ClockSeq = micros_since_epoch(os:timestamp()),
            UtcIdSuffix = Suffix, %config:get("uuids", "utc_id_suffix", ""),
            {utc_id, UtcIdSuffix, ClockSeq};
        Unknown -> throw({unknown_uuid_algorithm, Unknown})
    end.

% @private
micros_since_epoch({_, _, Micro} = Now) ->
    Nowish = calendar:now_to_universal_time(Now),
    Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
    Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    (Nowsecs - Then) * 1000000 + Micro.

% @private
utc_random(ClockSeq) ->
    Suffix = to_hex(crypto:strong_rand_bytes(9)),
    utc_suffix(Suffix, ClockSeq, os:timestamp()).

% @private
utc_suffix(Suffix, ClockSeq, Now) ->
    OsMicros = micros_since_epoch(Now),
    NewClockSeq = if
        OsMicros =< ClockSeq ->
            % Timestamp is lagging, use ClockSeq as Timestamp
            ClockSeq + 1;
        OsMicros > ClockSeq ->
            % Timestamp advanced, use it, and reset ClockSeq with it
            OsMicros
    end,
    Prefix = io_lib:format("~14.16.0b", [NewClockSeq]),
    {list_to_binary(Prefix ++ Suffix), NewClockSeq}.




%%
%%  Original: https://github.com/apache/couchdb/blob/master/src/couch/src/couch_util.erl
%%

% @private
to_hex(<<Hi:4, Lo:4, Rest/binary>>) ->
    [nibble_to_hex(Hi), nibble_to_hex(Lo) | to_hex(Rest)];
to_hex(<<>>) ->
    [];
to_hex(List) when is_list(List) ->
    to_hex(list_to_binary(List)).

% @private
nibble_to_hex(0) -> $0;
nibble_to_hex(1) -> $1;
nibble_to_hex(2) -> $2;
nibble_to_hex(3) -> $3;
nibble_to_hex(4) -> $4;
nibble_to_hex(5) -> $5;
nibble_to_hex(6) -> $6;
nibble_to_hex(7) -> $7;
nibble_to_hex(8) -> $8;
nibble_to_hex(9) -> $9;
nibble_to_hex(10) -> $a;
nibble_to_hex(11) -> $b;
nibble_to_hex(12) -> $c;
nibble_to_hex(13) -> $d;
nibble_to_hex(14) -> $e;
nibble_to_hex(15) -> $f.

