-module(erlcache_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Start = erlcache_sup:start_link(),
    ok = start_backend(),
    ok = start_frontend(),
    Start.

stop(_State) ->
    ok.

start_backend() ->  
    ok = riak_core:register_vnode_module(erlcache_vnode),
    ok = riak_core_node_watcher:service_up(erlcache, self()),
    ok.

start_frontend() ->
    ok = erlcache_cowboy_listener:start(),
    ok.
