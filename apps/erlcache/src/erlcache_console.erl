%% https://github.com/rzezeski/try-try-try/blob/master/2011/riak-core-first-multinode/mfmn/src/mfmn_console.erl
-module(erlcache_console).
-export([join/1,
         leave/1,
         remove/1,
         ringready/1]).

join([NodeStr]) ->
    riak_core:join(NodeStr).

leave([]) ->
    riak_core:leave().

remove([Node]) ->
    riak_core:remove(Node).

-spec(ringready([]) -> ok | error).
ringready([]) ->
    case ringready() of
        {ok, Nodes} ->
            io:format("TRUE All nodes agree on the ring: ~p~n", [Nodes]);
        {error, {different_owners, N1, N2}} ->
            io:format("FALSE Node ~p and ~p list different partition owners~n", [N1, N2]),
            error;
        {error, {nodes_down, Down}} ->
            io:format("FALSE ~p down.  All nodes need to be up to check.~n", [Down]),
            error
    end.

-spec(ringready() -> {ok, [atom()]} | {error, any()}).
ringready() ->
    case get_rings() of
        {[], Rings} ->
            {N1,R1}=hd(Rings),
            case rings_match(hash_ring(R1), tl(Rings)) of
                true ->
                    Nodes = [N || {N,_} <- Rings],
                    {ok, Nodes};

                {false, N2} ->
                    {error, {different_owners, N1, N2}}
            end;

        {Down, _Rings} ->
            io:format("Rings down: ~p~n~p~n", [Down, _Rings]),
            {error, {nodes_down, Down}}
    end.

%% Retrieve the rings for all other nodes by RPC
get_rings() ->
    {RawRings, Down} = riak_core_util:rpc_every_member(
                         riak_core_ring_manager, get_my_ring, [], 30000),
    Rings = orddict:from_list([{riak_core_ring:owner_node(R), R} || {ok, R} <- RawRings]),
    {lists:sort(Down), Rings}.

%% Produce a hash of the 'chash' portion of the ring
hash_ring(R) ->
    erlang:phash2(riak_core_ring:all_owners(R)).

%% Check if all rings match given a hash and a list of [{N,P}] to check
rings_match(_, []) ->
    true;
rings_match(R1hash, [{N2, R2} | Rest]) ->
    case hash_ring(R2) of
        R1hash ->
            rings_match(R1hash, Rest);
        _ ->
            {false, N2}
    end.
