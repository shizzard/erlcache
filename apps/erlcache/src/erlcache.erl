-module(erlcache).
-include("erlcache.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0]).

-export([env/1,env/2,env/3]).

ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, erlcache),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, erlcache_vnode_master).

env(Keys) ->
    env(Keys, undefined).
env([], Default) ->
    Default;
env([Key | Keys], Default) ->
    env(Keys, Default, application:get_env(erlcache, Key)).
env(_Keys, Default, undefined) ->
    Default;
env([Key | Keys], Default, {ok, Proplist}) ->
    env(Keys, Default, {ok, proplists:get_value(Key, Proplist)});
env([], _Default, {ok, Value}) ->
    Value.