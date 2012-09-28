-module(trade_fsm).
-behaviour(gen_fsm).

% Public API %
-export([start/1, start_link/1, trade/2, accept_trade/1,
        make_offer/2, retract_offer/2, ready/1, cancel/1]).

% Gen fsm callbacks %
-export([init/1, handler_event/3, handle_sync_event/4,
        handle_info/3, terminate/3, code_change/4]).

%% Public API %%
%% User -> fsm funcs %%
start(Name) ->
    gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
    gen_fsm:start_link(?MODULE, [Name], []).

%: Ask to start a trading session. Returns if/when the other accepts
trade(OwnPid, OtherPid) ->
    gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

%: Accept a trade offer
accept_trade(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, accept_negotiate).

%: Send an item to be traded
make_offer(OwnPid, Item) ->
    gen_fsm:send_event(OwnPid, {make_offer, Item}).

%: Cancel trade of an item
retract_offer(OwnPid, Item) ->
    gen_fsm:send_event(OwnPid, {retract_offer, Item}).

%: We're happy with the trade. If the other player
%  also declares ready, the trade is done.
ready(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, ready, infinity).

%: Cancel the transaction
cancel(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, cancel).

%% Fsm -> fsm funcs %
%: Ask remote for trade session
ask_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

%: Forward user -> fsm accept message
accept_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

%: Forward user offer
do_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {do_offer, Item}).

%: Forward cancellation
undo_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {undo_offer, Item}).

%: Request other side to finalise
are_you_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, are_you_ready).

%: No, not ready to finish yet
not_yet(OtherPid) ->
    gen_fsm:send_event(OtherPid, not_yet).

%: We have finalised, all ready
am_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, 'ready!').

%% Transaction completion %%
%: Acknowledge the fsm is in ready state
ack_trans(OtherPid) ->
    gen_fsm:send_event(OtherPid, ack).

%: Query ready to commit
ask_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, ask_commit).

%: Begin synchronous commit
do_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, do_commit).

%: courtesy cancellation notification
notify_cancel(OtherPid) ->
    gen_fsm:send_all_state_event(OtherPid, cancel).


%% GEN FSM calls %%
-record(state, 
        {name="",
         other,
         ownitems=[],
         otheritems=[],
         monitor,
         from}).

init(Name) ->
    {ok, idle, #state{name=Name}}.

%% Notification
notice(#state{name=N}, Str, Args) ->
    io:format("~s: "++Str++"~n", [N|Args]).

%% Log dead messages
unexpected(Msg, State) ->
    io:format("~p received unknown event ~p while in state ~p~n",
        [self(), Msg, State]).

%% add item to list
add(Item, Items) -> [Item | Items].

%% remove item from list
remove(Item, Items) -> Items -- [Item].

%: async - ask_negotiate
idle({ask_negotiate, OtherPid}, S=#state{}) ->
    Ref = monitor(process, OtherPid),
    notice(S, "~p asked for a trade negotiation", [OtherPid]),
    {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref}};
idle(Event, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.

%: sync - start negotiating
idle({negotiate, OtherPid}, From, S=#state{}) ->
    ask_negotiate(OtherPid, self()),
    notice(S, "asking user ~p for a trade", [OtherPid]),
    Ref = monitor(process, OtherPid),
    {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref, from=From}};
idle(Event, _From,  Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.

idle_wait({ask_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
    gen_fsm:reply(S#state.from, ok),
    notice(S, "starting negotiation", []),
    {next_state, negotiate, S};
idle_wait({accept_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
    gen_fsm:reply(S#state.from, ok),
    notice(S, "starting negotiation", []),
    {next_state, negotiate, S};
idle_wait(Event, Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.

idle_wait(accept_negotiate, _From, S=#state{other=OtherPid}) ->
    accept_negotiate(OtherPid, self()),
    notice(S, "accepting negotiation", []),
    {reply, ok, negotiate, S};
idle_wait(Event, _From, Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.

%% We make initial offer
negotiate({make_offer, Item}, S=#state{ownitems=OwnItems}) ->
    do_offer(S#state.other, Item),
    notice(S, "offering ~p", [Item]),
    {next_state, negotiate, S#state{ownitems=add(Item, OwnItems)}};
%% We retract an offer
negotiate({retract_offer, Item}, S=#state{ownitems=OwnItems}) ->
    undo_offer(S#state.other, Item),
    notice(S, "cancelling offer on ~p", [Item]),
    {next_state, negotiate, S#state{ownitems=remove(Item, OwnItems)}};
%% Other side make an offer
negotiate({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
    notice(S, "other player offering ~p", [Item]),
    {next_state, negotiate, S#state{otheritems=add(Item, OtherItems)}};
negotiate({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
    notice(S, "other player cancelling offer on ~p", [Item]),
    {next_state, negotiate, S#state{otheritems=remove(Item, OtherItems)}};

%% other side signaled they're done trading
negotiate(are_you_ready, S=#state{other=OtherPid}) ->
    io:format("Other user ready to trade.~n"),
    notice(S,
        "Other user ready to transfer goods:~n"
        "You get ~p, they get ~p",
        [S#state.otheritems, S#state.ownitems]),
    not_yet(OtherPid),
    {next_state, negotiate, S};
negotiate(Event, Data) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, Data}.

%% Synchronous wait for ready
negotiate(ready, From, S = #state{other=OtherPid}) ->
    are_you_ready(OtherPid),
    notice(S, "asking if ready, waiting", []),
    {next_state, wait, S#state{from=From}};
negotiate(Event, _From, S) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, S}.



