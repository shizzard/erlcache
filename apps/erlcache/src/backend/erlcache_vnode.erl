-module(erlcache_vnode).
-behaviour(riak_core_vnode).
-include("erlcache.hrl").
-include("erlcache_vnode.hrl").

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

-record(state, {
    partition,
    dict
}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state { 
        partition=Partition,
        dict = dict:new()
    }}.

% Sample command: respond to a ping
handle_command(ping, Sender, State) ->
    ?INFO(
        "~p-VNODE-CALL => handle_command: ~nMessage: ~p~nSender: ~p~nState:~p",
        [self(), ping, Sender, State]
    ),
    {reply, {pong, State#state.partition}, State};
handle_command(?GET_COMMAND(Key), Sender, State = #state{dict = Dict0}) ->
    ?INFO(
        "~p-VNODE-CALL => handle_command: ~nMessage: ~p~nSender: ~p~nState:~p",
        [self(), ?GET_COMMAND(Key), Sender, State]
    ),
    Reply = case dict:find(Key, Dict0) of 
        {ok, Value} -> 
            {ok, Value};
        error ->
            {error, not_found}
    end,
    {reply, Reply, State};
handle_command(?SET_COMMAND(Key, Value), Sender, State = #state{dict = Dict0}) ->
    ?INFO(
        "~p-VNODE-CALL => handle_command: ~nMessage: ~p~nSender: ~p~nState:~p",
        [self(), ?SET_COMMAND(Key, Value), Sender, State]
    ),
    Dict1 = dict:store(Key, Value, Dict0),
    {reply, {ok, set}, State#state{dict = Dict1}};
handle_command(Message, Sender, State) ->
    ?WARNING(
        "Invalid handle_command call: ~nMessage: ~p~nSender: ~p~nState:~p",
        [Message, Sender, State]
    ),
    {noreply, State}.

handle_coverage(Request, KeySpaces, Sender, State) -> 
    ?INFO(
        "~p-VNODE-CALL => handle_coverage: ~nRequest: ~p~nKeyspaces: ~p~nSender: ~p~nState:~p",
        [self(), Request, KeySpaces, Sender, State]
    ),
    {continue, State}.

handle_exit(Pid, Reason, State) ->
    ?INFO(
        "~p-VNODE-CALL => handle_exit: ~nPid: ~p~nReason: ~p~nState:~p",
        [self(), Pid, Reason, State]
    ),
    {stop, Reason, State}.

handle_handoff_command(Message, Sender, State) ->
    ?INFO(
        "~p-VNODE-CALL => handle_handoff_command: ~nMessage: ~p~nSender: ~p~nState:~p",
        [self(), Message, Sender, State]
    ),
    {noreply, State}.

handoff_starting(TargetNode, State) ->
    ?INFO(
        "~p-VNODE-CALL => handdoff_starting: ~nTargetNode: ~p~nState:~p",
        [self(), TargetNode, State]
    ),
    {true, State}.

handoff_cancelled(State) ->
    ?INFO(
        "~p-VNODE-CALL => handoff_cancelled: ~nState:~p",
        [self(), State]
    ),
    {ok, State}.

handoff_finished(TargetNode, State) ->
    ?INFO(
        "~p-VNODE-CALL => handoff_finished: ~nTargetNode: ~p~nState:~p",
        [self(), TargetNode, State]
    ),
    {ok, State}.

handle_handoff_data(Data, State = #state{dict = Dict0}) ->
    ?INFO(
        "~p-VNODE-CALL => handle_handoff_data: ~nData: ~p~nState:~p",
        [self(), Data, State]
    ),
    {Key, Value} = binary_to_term(Data), 
    Dict1 = dict:append(Key, Value, Dict0),
    {reply, ok, State#state{dict = Dict1}}.

encode_handoff_item(Key, Value) ->
    ?INFO(
        "~p-VNODE-CALL => encode_hadoff_item: ~nKey: ~p~nValue:~p",
        [self(), Key, Value]
    ),
    term_to_binary({Key, Value}).

is_empty(State = #state{dict = Dict0}) ->
    ?INFO(
        "~p-VNODE-CALL => is_empty: ~nState:~p",
        [self(), State]
    ),
    {0 == dict:size(Dict0), State}.

delete(State) ->
    ?INFO(
        "~p-VNODE-CALL => delete: ~nState:~p",
        [self(), State]
    ),
    {ok, State}.

terminate(Reason, State) ->
    ?INFO(
        "~p-VNODE-CALL => terminate: ~nReason: ~p~nState:~p",
        [self(), Reason, State]
    ),
    ok.
