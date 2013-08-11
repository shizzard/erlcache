-module(erlcache_vnode).
-behaviour(riak_core_vnode).
-include("erlcache.hrl").

-export([
    start_vnode/1,
    init/1,
    terminate/2,
    handle_command/3,
    is_empty/1,
    delete/1,
    handle_coverage/4,
    handle_exit/3,
    handle_handoff_command/3,
    handoff_starting/2,
    handoff_cancelled/1,
    handoff_finished/2,
    handle_handoff_data/2,
    encode_handoff_item/2
]).

-record(state, {partition}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state { partition=Partition }}.

% Sample command: respond to a ping
handle_command(ping, Sender, State) ->
    ?INFO(
        "VNODE-CALL => handle_command: ~nMessage: ~p~nSender: ~p~nState:~p",
        [ping, Sender, State]
    ),
    {reply, {pong, State#state.partition}, State};
handle_command(Message, Sender, State) ->
    ?WARNING(
        "handle_command: ~nMessage: ~p~nSender: ~p~nState:~p",
        [Message, Sender, State]
    ),
    {noreply, State}.

handle_coverage(Request, KeySpaces, Sender, State) -> 
    ?INFO(
        "VNODE-CALL => handle_coverage: ~nRequest: ~p~nKeyspaces: ~p~nSender: ~p~nState:~p",
        [Request, KeySpaces, Sender, State]
    ),
    {continue, State}.

handle_exit(Pid, Reason, State) ->
    ?INFO(
        "VNODE-CALL => handle_exit: ~nPid: ~p~nReason: ~p~nState:~p",
        [Pid, Reason, State]
    ),
    {stop, Reason, State}.

handle_handoff_command(Message, Sender, State) ->
    ?INFO(
        "VNODE-CALL => handle_handoff_command: ~nMessage: ~p~nSender: ~p~nState:~p",
        [Message, Sender, State]
    ),
    {noreply, State}.

handoff_starting(TargetNode, State) ->
    ?INFO(
        "VNODE-CALL => handdoff_starting: ~nTargetNode: ~p~nState:~p",
        [TargetNode, State]
    ),
    {true, State}.

handoff_cancelled(State) ->
    ?INFO(
        "VNODE-CALL => handoff_cancelled: ~nState:~p",
        [State]
    ),
    {ok, State}.

handoff_finished(TargetNode, State) ->
    ?INFO(
        "VNODE-CALL => handoff_finished: ~nTargetNode: ~p~nState:~p",
        [TargetNode, State]
    ),
    {ok, State}.

handle_handoff_data(Data, State) ->
    ?INFO(
        "VNODE-CALL => handle_handoff_data: ~nData: ~p~nState:~p",
        [Data, State]
    ),
    {reply, ok, State}.

encode_handoff_item(Key, Value) ->
    ?INFO(
        "VNODE-CALL => encode_hadoff_item: ~nKey: ~p~nValue:~p",
        [Key, Value]
    ),
    <<>>.

is_empty(State) ->
    ?INFO(
        "VNODE-CALL => is_empty: ~nState:~p",
        [State]
    ),
    {true, State}.

delete(State) ->
    ?INFO(
        "VNODE-CALL => delete: ~nState:~p",
        [State]
    ),
    {ok, State}.

terminate(Reason, State) ->
    ?INFO(
        "VNODE-CALL => terminate: ~nReason: ~p~nState:~p",
        [Reason, State]
    ),
    ok.
