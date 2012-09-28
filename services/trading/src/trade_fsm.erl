-module(trade_fsm).
-behaviour(gen_fsm).

% Public API %
-export([start/1, start_link/1, trade/2, accept_trade/1,
        make_offer/2, retract_offer/2, ready/1, cancel/1]).

% Gen fsm callbacks %
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4,
        %custom states
        idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2,
        negotiate/3, wait/2, ready/2, ready/3]).

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

%: Forward ca3, ncellation
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

%% Handle offers and retractions while waiting by returning to
%  negotiation
wait({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
    gen_fsm:reply(S#state.from, offer_changed),
    notice(S, "other side offering ~p", [Item]),
    {next_state, negotiate, S#state{otheritems=add(Item, OtherItems)}};
wait({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
    gen_fsm:reply(S#state.from, offer_changed),
    notice(S, "other side cancelling offer of ~p", [Item]),
    {next_state, negotiate, S#state{otheritems=remove(Item, OtherItems)}};
%% Handshake for all ready
wait(are_you_ready, S=#state{}) ->
    am_ready(S#state.other),
    notice(S, "asked if ready, and I am. Waiting for same reply", []),
    {next_state, wait, S};
%% Continue waiting if other side not ready but no offer changes
wait(not_yet, S=#state{}) ->
    notice(S, "Other side not ready yet", []),
    {next_state, wait, S};

%% Finish handshake - both sides ready
wait('ready!', S=#state{}) ->
    am_ready(S#state.other),
    ack_trans(S#state.other),
    gen_fsm:reply(S#state.from, ok),
    notice(S, "other side ready, moving to ready state", []),
    {next_state, ready, S};

%% Dead messages
wait(Event, Data) ->
    unexpected(Event, wait),
    {next_state, wait, Data}.

priority(OwnPid, OtherPid) when OwnPid > OtherPid -> true;
priority(OwnPid, OtherPid) when OwnPid < OtherPid -> false.
commit(S=#state{}) ->
    io:format(
        "Transaction completed for ~s. "
        "Items sent are:~n~p,~n received are:~n~p,~n",
        [S#state.name, S#state.ownitems, S#state.otheritems]).

%% Start or wait for commit, based on pid priority
ready(ack, S=#state{}) ->
    case priority(self(), S#state.other) of
        false ->
            {next_state, ready, S};
        true ->
            try
                notice(S, "asking for commit", []),
                ready_commit = ask_commit(S#state.other),
                notice(S, "ordering commit", []),
                ok = do_commit(S#state.other),
                notice(S, "committing...", []),
                commit(S),
                {stop, normal, S}
            catch Class:Reason ->
                notice(S, "commit failed", []),
                {stop, {Class, Reason}, S}
            end
    end;
ready(Event, Data) ->
    unexpected(Event, ready),
    {next_state, ready, Data}.

%% Commit stages
ready(ask_commit, _From, S) ->
    notice(S, "replying to ask_commit", []),
    {reply, ready_commit, ready, S};
ready(do_commit, _From, S) ->
    notice(S, "committing...", []),
    commit(S),
    {stop, normal, ok, S};
ready(Event, _From, Data) ->
    unexpected(Event, ready),
    {next_state, ready, Data}.

%% Other side is cancelling. Drop everything and shutdown.
handle_event(cancel, _StateName, S=#state{}) ->
    notice(S, "received cancel event", []),
    {stop, other_cancelled, S};
handle_event(Event, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

%% Tell other side if we quit
handle_sync_event(cancel, _From, _StateName, S=#state{}) ->
    notify_cancel(S#state.other),
    notice(S, "cancelling trade, sending cancel event",[]),
    {stop, cancelled, ok, S};
% no reply to unexpected sync events. Let the other side crash.
handle_sync_event(Event, _From, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

%% Cope with the other fsm dying
handle_info({'DOWN', Ref, process, Pid, Reason}, _, S=#state{other=Pid,
        monitor=Ref}) ->
    notice(S, "other side dead", []),
    {stop, {other_down, Reason}, S};
handle_info(Info, StateName, Data) ->
    unexpected(Info, StateName),
    {next_state, StateName, Data}.

%% All done
terminate(normal, ready, S=#state{}) ->
    notice(S, "FSM leaving", []);
terminate(_Reason, _StateName, _StateData) ->
    ok.

%% Change teh codez
code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

