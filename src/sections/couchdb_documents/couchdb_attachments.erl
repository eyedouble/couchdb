-module(couchdb_attachments).

-include("couchdb.hrl").
-include("../../dev.hrl").

-export([
    fetch/3
    ,fetch/4
    ,stream/1
    ,delete/3
    ,delete/4
    ,put/4
    ,put/5
    ,send/2
]).


%% @doc fetch a document attachment
%% @equiv fetch(Db, DocId, Name, [])
fetch(Db, DocId, Name) ->
    fetch(Db, DocId, Name, []).

%% </ul>
%%
-spec fetch(db(), string(), string(),
                       list())
    -> {ok, binary()}| {ok, atom()} |{error, term()}.
fetch(#db{server=Server, options=Opts}=Db, DocId, Name, Options0) ->
    {Stream, Options} = case couchdb_util:get_value(stream, Options0) of
        undefined ->
            {false, Options0};
        true ->
            {true, proplists:delete(stream, Options0)};
        _ ->
            {false, proplists:delete(stream, Options0)}
    end,


    Options1 = couchdb_util:parse_options(Options),

    %% custom headers. Allows us to manage Range.
    {Options2, Headers} = case couchdb_util:get_value("headers", Options1) of
        undefined ->
            {Options1, []};
        Headers1 ->
            {proplists:delete("headers", Options1), Headers1}
    end,

    DocId1 = couchdb_util:encode_docid(DocId),
    Url = hackney_url:make_url(couchdb_httpc:server_url(Server),
                               [couchdb_httpc:db_url(Db), DocId1,
                                Name],
                               Options2),
    case hackney:get(Url, Headers, <<>>, Opts) of
        {ok, 200, _, Ref} when Stream /= true ->
            hackney:body(Ref);
        {ok, 200, _, Ref} ->
            {ok, Ref};
        {ok, 404, _, Ref} ->
            hackney:skip_body(Ref),
            {error, not_found};
        {ok, Status, Headers, Ref} ->
            {ok, Body} = hackney:body(Ref),
            {error, {bad_response, {Status, Headers, Body}}};

        Error ->
            Error
    end.

%% @doc fetch an attachment chunk.
%% Use this function when you pass the `stream' option to the
%% `couchdb:fetch/4' function.
%% This function return the following response:
%%      <dl>
%%          <dt>done</dt>
%%              <dd>You got all the attachment</dd>
%%          <dt>{ok, binary()}</dt>
%%              <dd>Part of the attachment</dd>
%%          <dt>{error, term()}</dt>
%%              <dd>n error occurred</dd>
%%      </dl>
%%

-spec stream(atom()) -> {ok, binary()}
    | done
    | {error, term()}.
stream(Ref) ->
    hackney:stream_body(Ref).

%% @doc put an attachment
%% @equiv put(Db, Document, Name, Body, [])
put(Db, Document, Name, Body) ->
    put(Db, Document, Name, Body, []).

%% @doc put an attachment
%
%%                      Body::body(), Option::optionList()) -> {ok, iolist()}
%%       optionList() = [option()]
%%       option() = {rev, string()} |
%%                  {content_type, string()} |
%%                  {content_length, string()}
%%       body() = [] | string() | binary() | fun_arity_0() |
%%       {fun_arity_1(), initial_state(), stream}
%%       initial_state() = term()
-spec(put(Db::db(), Document::map(), Name::binary(), Body::binary(), Options::list()) -> {ok, term()}).
put(#db{server=Server, options=Opts}=Db, #{<<"_id">> := Id, <<"_rev">> := Rev}=_Doc, <<Name/binary>>, Body, Options) ->
    Headers = couchdb_util:get_value(headers, Options, []),
    FinalHeaders = lists:foldl(fun(Option, Acc) ->
        case Option of
                {content_length, V} ->
                    V1 = couchdb_util:to_binary(V),
                    [{<<"Content-Length">>, V1}|Acc];
                {content_type, V} ->
                    V1 = couchdb_util:to_binary(V),
                    [{<<"Content-Type">>, V1}|Acc];
                _ ->
                    Acc
        end
    end, Headers, Options),
    QueryArgs = #{<<"rev">> => Rev},
    
    DocId1 = couchdb_util:encode_docid(Id),
    {ok, AttName} = couchdb_util:encode_att_name(Name),

    Url = hackney_url:make_url(
        couchdb_httpc:server_url(Server), 
        [couchdb_httpc:db_url(Db), DocId1,AttName],
        maps:to_list(QueryArgs)
    ),

    case couchdb_httpc:db_request(put, Url, FinalHeaders, Body, Opts, [201]) of
        {ok, _, _, Ref} ->
            JsonBody = couchdb_httpc:json_body(Ref),
            #{<<"ok">> := true, <<"id">> := RId, <<"rev">> := RRev}=_R = JsonBody,
            {ok, #{<<"_id">> => RId, <<"_rev">> => RRev}};
        {ok, Ref} ->
            {ok, Ref};
        Error ->
            Error
    end.

%% @doc send an attachment chunk
%% Msg could be Data, eof to stop sending.
send(Ref, eof) ->
    case hackney:finish_send_body(Ref) of
        ok ->
            Resp =  hackney:start_response(Ref),
            couchdb_httpc:reply_att(Resp);
        Error ->
            Error
    end;
send(Ref, Msg) ->
    Reply = hackney:send_body(Ref, Msg),
    couchdb_httpc:reply_att(Reply).


%% @doc delete a document attachment
%% @equiv delete(Db, Doc, Name, [])
delete(Db, Doc, Name) ->
    delete(Db, Doc, Name, []).

%% @doc delete a document attachment
%% @spec(db(), string()|list(), string(), list() -> {ok, Result} | {error, Error}
delete(#db{server=Server, options=Opts}=Db, DocOrDocId, Name,
                  Options) ->
    Options1 = couchdb_util:parse_options(Options),
    {Rev, DocId} = case DocOrDocId of
        {Props} ->
            Rev1 = couchdb_util:get_value(<<"_rev">>, Props),
            DocId1 = couchdb_util:get_value(<<"_id">>, Props),
            {Rev1, DocId1};
        DocId1 ->
            Rev1 = couchdb_util:get_value("rev", Options1),
            {Rev1, DocId1}
    end,
    case Rev of
        undefined ->
           {error, rev_undefined};
        _ ->
            Options2 = case couchdb_util:get_value("rev", Options1) of
                undefined ->
                    [{<<"rev">>, couchdb_util:to_binary(Rev)}|Options1];
                _ ->
                    Options1
            end,
            Url = hackney_url:make_url(couchdb_httpc:server_url(Server), [couchdb_httpc:db_url(Db),
                                                            DocId,
                                                            Name],
                                       Options2),

            case couchdb_httpc:db_request(delete, Url, [], <<>>, Opts,
                                            [200]) of
            {ok, _, _, Ref} ->
                {[{<<"ok">>,true}|R]} = couchdb_httpc:json_body(Ref),
                {ok, {R}};
            Error ->
                Error
            end
    end.