-module(erlcache).
-include("erlcache.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0]).

ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, erlcache),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, erlcache_vnode_master).
