-module(erlcache_cowboy_listener).

-include("erlcache.hrl").

-export([start/0]).

-define(DEFAULT_PORT, 8000).
-define(DEFAULT_LISTENERS, 100).

start() ->
    Port = erlcache:env([cowboy_options, port], ?DEFAULT_PORT),
    Listeners = erlcache:env([cowboy_options, listeners], ?DEFAULT_LISTENERS),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, [
                {directory, {priv_dir, erlcache, [<<"static">>]}},
                {file, "index.html"},
                {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
            ]},
            {"/action", erlcache_cowboy_handler, []},
            {"/[...]", cowboy_static, [
                {directory, {priv_dir, erlcache, [<<"static">>]}},
                {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
            ]}
        ]}
    ]),
    {ok, _} = cowboy:start_http(?MODULE, Listeners,
        [{port, Port}],
        [{env, [
            {dispatch, Dispatch}
        ]}]
    ),
    ?INFO("Listening on port ~p", [Port]),
    ok.
