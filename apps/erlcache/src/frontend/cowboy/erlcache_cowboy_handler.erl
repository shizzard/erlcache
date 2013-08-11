-module(erlcache_cowboy_handler).

-export([init/3, content_types_provided/2]).
-export([to_json/2]).
-export([terminate/3]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json}
    ], Req, State}.

to_json(Req0, State) ->
    {<<"{\"result\":\"Erlcache ok\"}">>, Req0, State}.

terminate(_Reason, _Req, _State) ->
    ok.
