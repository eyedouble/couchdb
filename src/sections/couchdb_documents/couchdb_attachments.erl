%% @doc The `couchdb_attachments' module contains functionality listed under CouchDB API
%% Reference section 1.4.2.
%% CouchDB documents can have attachments just like an email message can have attachments. 
%% An attachment is identified by a name and includes its MIME type (or Content-Type) and the 
%% number of bytes the attachment contains. Attachments can be any data. It is easiest to think 
%% about attachments as files attached to a document. These files can be text, images, Word 
%% documents, music, or movie files.
%%
%%
-module(couchdb_attachments).

-include("couchdb.hrl").
-include("../../dev.hrl").

-export([
    get/3
    ,get/4
    ,get_stream/3
    ,get_stream/4
    ,stream/1
    ,delete/3
    ,delete/4
    ,put/4
    ,put/5
    ,send/2
]).


%% @doc Get a document attachment
%% @equiv get(Db, DocId, Name, [])
-spec(get(db(), map(), binary()) -> {ok, binary()}| {ok, map()} |{error, term()}).
get(Db, Doc, Name) -> get(Db, Doc, Name, #{}).

%% @doc Get a document attachment
%%
%% ```
%% Option = #{<<"range">> => <<"0-19">>};
%% '''
%%
%%  Option above will allow you to only fetch the first 19 bytes of
%%  the attachment.
%%
-spec(get(db(), map(), binary(), list()) -> {ok, binary()}| {ok, map()} |{error, term()}).
get(#db{server=#server{url=ServerUrl}, name=DbName, options=Opts}, #{<<"_id">> := <<DocId/binary>>, <<"_rev">> := <<_Rev/binary>>}=_Doc, <<Name/binary>>, #{}=Options) ->
    Headers = case Options of
        #{<<"range">> := <<Value/binary>>} ->[{<<"Range">>, <<"bytes=", Value>>}];
        _Other -> []
    end,
    
    Url = hackney_url:make_url(ServerUrl, <<DbName/binary,"/",DocId/binary,"/",Name/binary>>, maps:to_list(get_req_filter_queries(Options))),
    case hackney:get(Url, Headers, <<>>, Opts) of        
        {ok, 200, _, Ref} -> hackney:body(Ref);
        {ok, 404, _, Ref} -> hackney:skip_body(Ref), {error, not_found};
        {ok, Status, Headers, Ref} -> {ok, Body} = hackney:body(Ref), {error, {bad_response, {Status, Headers, Body}}};
        Error -> Error
    end.


%% @doc Get a document attachment streaming
%% @equiv get_stream(Db, DocId, Name, [])
-spec(get_stream(db(), map(), binary()) -> {ok, binary()}| {ok, map()} |{error, term()}).
get_stream(Db, Doc, Name) -> get(Db, Doc, Name, #{}).

%% @doc Get a document attachment streaming
%%
%% ```
%% Option = #{<<"range">> => <<"0-19">>};
%% '''
%%
%%  Option above will allow you to only fetch the first 19 bytes of
%%  the attachment.
%%
-spec(get_stream(db(), string(), string(), list()) -> {ok, binary()}| {ok, reference()} |{error, term()}).
get_stream(#db{server=#server{url=ServerUrl}, name=DbName, options=Opts}, #{<<"_id">> := <<DocId/binary>>, <<"_rev">> := <<_Rev/binary>>}=_Doc, <<Name/binary>>, #{}=Options) -> 
    Headers = case Options of
        #{<<"range">> := <<Value/binary>>} ->[{<<"Range">>, <<"bytes=", Value>>}];
        _Other -> []
    end,
    
    Url = hackney_url:make_url(ServerUrl, <<DbName/binary,"/",DocId/binary,"/",Name/binary>>, maps:to_list(get_req_filter_queries(Options))),
    case hackney:get(Url, Headers, <<>>, Opts) of
        {ok, 200, _, Ref} -> {ok, Ref};
        {ok, 404, _, Ref} -> hackney:skip_body(Ref), {error, not_found};
        {ok, Status, Headers, Ref} -> {ok, Body} = hackney:body(Ref), {error, {bad_response, {Status, Headers, Body}}};
        Error -> Error
    end.



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




%% @doc delete a document attachment
%% @equiv delete(Db, Doc, Name, [])
delete(Db, Doc, Name) ->
    delete(Db, Doc, Name, #{}).

%% @doc delete a document attachment
%% @spec(db(), string()|list(), string(), list() -> {ok, Result} | {error, Error}
-spec(delete(Db::db(), Document::map(), Name::binary(), Options::list()) -> {ok, term()}).
delete(#db{server=#server{url=ServerUrl}, name=DbName, options=Opts}, #{<<"_id">> := <<DocId/binary>>, <<"_rev">> := <<Rev/binary>>}=_Doc, <<Name/binary>>, #{}=Options) ->  
    Url = hackney_url:make_url(ServerUrl, [DbName, DocId, Name], [{<<"rev">>, Rev}|maps:to_list(Options)]),
    case couchdb_httpc:db_request(delete, Url, [], <<>>, Opts, [200]) of
        {ok, _, _, Ref} ->
            JsonBody = couchdb_httpc:json_body(Ref),
            #{<<"ok">> := true, <<"id">> := RId, <<"rev">> := RRev}=_R = JsonBody,
            {ok, #{<<"_id">> => RId, <<"_rev">> => RRev}};
        Error -> Error
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
%% Not tested
-spec(stream(reference()) -> {ok, binary()} | done | {error, term()}).
stream(Ref) -> hackney:stream_body(Ref).

%% @doc send an attachment chunk
%% Msg could be Data, eof to stop sending.
%% Not tested
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


%
%   Internal
%
%% @private
get_req_filter_queries(Qs) ->
    QsDefinition = #{
        <<"rev">> => fun(X) -> is_binary(X) end,
        <<"batch">> => fun(X) -> is_binary(X) end
    }, 
    maps:fold(fun(Key, Value, Acc)->      
        case maps:get(Key, QsDefinition) of
            Check when is_function(Check, 1) -> case Check(Value) of
                true -> maps:merge(Acc, #{Key => Value});
                false -> Acc
            end;
            _Other -> false
        end

    end, #{}, Qs);
%% @private
get_req_filter_queries(_) -> false.