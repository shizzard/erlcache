-module(erlcache_cowboy_handler).

-include("erlcache.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(HEADERS, [
    {<<"Content-Type">>, <<"application/json">>}
]).

-record(state, {
    method, 
    post_key,
    post_value, 
    get_key
}).


init(_Transport, Req0, []) ->
    {ok, BodyQs, Req1} = cowboy_req:body_qs(Req0), 
    PostKey = proplists:get_value(<<"key">>, BodyQs),
    PostValue = proplists:get_value(<<"value">>, BodyQs),
    {GetKey, Req2} = cowboy_req:qs_val(<<"key">>, Req1),
    {Method, Req3} = cowboy_req:method(Req2),
    State = #state{
        method = Method,
        post_key = PostKey,
        post_value = PostValue,
        get_key = GetKey
    },
    {ok, Req3, State}.

handle(Req0, State = #state{
    method = <<"GET">>, 
    get_key = Key
}) when is_binary(Key) ->
    Reply = case erlcache:get(Key) of 
        {ok, Value} ->
            encode_reply({get, Key, Value});
        {error, not_found} ->
            encode_error({Key, not_found})
    end,
    {ok, Req1} = cowboy_req:reply(200, ?HEADERS, Reply, Req0), 
    {ok, Req1, State};

handle(Req0, State = #state{
    method = <<"POST">>, 
    post_key = Key, 
    post_value = Value
}) when is_binary(Key), is_binary(Value) ->
    {ok, set} = erlcache:set(Key, Value),
    Reply = encode_reply({set, Key, Value}),
    {ok, Req1} = cowboy_req:reply(200, ?HEADERS, Reply, Req0), 
    {ok, Req1, State};

handle(Req0, State = #state{method = Method}) ->
    Reply = encode_error({invalid_request, Method}),
    {ok, Req1} = cowboy_req:reply(200, ?HEADERS, Reply, Req0), 
    {ok, Req1, State}.


terminate(_Reason, _Req, _State) ->
    ok.


encode_reply({get, Key, Value}) ->
    jiffy:encode({[
        {<<"operation">>, <<"GET">>},
        {<<"result">>, <<"ok">>},
        {<<"value">>, {[
            {Key, Value}
        ]}}
    ]});

encode_reply({set, Key, Value}) ->
    jiffy:encode({[
        {<<"operation">>, <<"POST">>},
        {<<"result">>, <<"ok">>},
        {<<"value">>, {[
            {Key, Value}
        ]}}
    ]}).


encode_error({invalid_request, Method}) ->
    jiffy:encode({[
        {<<"operation">>, Method},
        {<<"result">>, <<"error">>}
    ]});

encode_error({Key, not_found}) ->
    jiffy:encode({[
        {<<"operation">>, <<"POST">>},
        {<<"result">>, <<"error">>},
        {<<"value">>, {[
            {Key, <<"not_found">>}
        ]}}
    ]}).
