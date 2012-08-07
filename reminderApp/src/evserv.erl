-module(evserv).
%-compile(export_all).
-export([init/0, start/0, start_link/0, terminate/0, subscribe/1, add_event/3, cancel/1, listen/1]).
-record(state, {
            events,    %list of #event{}
            clients}). %list of Pid

-record(event, {name = "", description = "", pid, timeout={{1970,1,1},{0,0,0}}}).

init() ->
    loop(#state{
            events=orddict:new(),
            clients=orddict:new()}).

start() ->
    Pid = spawn(?MODULE, init, []),
    register(?MODULE, Pid),
    Pid.

start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    register(?MODULE, Pid),
    Pid.

terminate() ->
    ?MODULE ! shutdown.

subscribe(Pid) ->
    Ref = erlang:monitor(process, whereis(?MODULE)),
    ?MODULE ! {self(), Ref, {subscribe, Pid}},
    receive
        {Ref, ok} ->
            {ok, Ref};
        {'DOWN', Ref, process, _Pid, Reason} ->
            {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

add_event(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
        {Ref, ok} ->
            {ok, Ref};
        {'DOWN', Ref, process, _Pid, Reason} ->
            {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

cancel(Name) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {cancel, Name}},
    receive
        {Ref, ok} -> ok
    after 5000 ->
        {error, timeout}
    end.

listen(ForSeconds) ->
    receive
        M = {done, _Name, _Description} ->
            [M | listen(0)]
    after ForSeconds*1000 ->
        []
    end.


% Innards %
loop(S = #state{}) ->
    receive
        % subscription
        {Pid, MsgRef, {subscribe, Client}} ->
            Ref = erlang:monitor(process, Client),
            NewClients = orddict:store(Ref, Client, S#state.clients),
            Pid ! {MsgRef, ok},
            loop(S#state{clients=NewClients});

        % add an event
        {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
            case valid_datetime(TimeOut) of
                true ->
                    EventPid = event:start_link(Name, TimeOut),
                    NewEvents = orddict:store(Name,
                                              #event{
                                                  name=Name,
                                                  description=Description,
                                                  pid=EventPid,
                                                  timeout=TimeOut},
                                              S#state.events),
                    Pid ! {MsgRef, ok},
                    loop(S#state{events=NewEvents});
                false ->
                    Pid ! {MsgRef, {error, bad_timeout}},
                    loop(S)
            end;

        % cancel an event
        {Pid, MsgRef, {cancel, Name}} ->
            Events =
                case orddict:find(Name, S#state.events) of
                    {ok, E} ->
                        event:cancel(E#event.pid),
                        orddict:erase(Name, S#state.events);
                    error ->
                        S#state.events
                end,
            Pid ! {MsgRef, ok},
            loop(S#state{events=Events});

        % event finished
        {done, Name} ->
            case orddict:find(Name, S#state.events) of
                {ok, E} ->
                    send_to_clients({done, E#event.name, E#event.description},
                                    S#state.clients),
                    NewEvents = orddict:erase(Name, S#state.events),
                    loop(S#state{events=NewEvents});
                error -> % race: firing a cancelled event
                    loop(S)
            end;

        % stop server
        shutdown ->
            exit(shutdown);

        % Client disconnected
        {'DOWN', Ref, process, _Pid, _Reason} ->
            loop(S#state{clients=orddict:erase(Ref, S#state.clients)});

        code_change ->
            ?MODULE:loop(S);

        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(S)
    end.

send_to_clients(Msg, ClientDict) ->
    orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

valid_datetime({Date, Time}) ->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        error:function_clause -> % if date not in {D,M,Y} format, or entire dt not {{,,},{,,}}.
            false
    end;
valid_datetime(_) ->
    false.

valid_time({H,M,S}) -> valid_time(H,M,S).
valid_time(H,M,S) when
                        H >= 0, H < 24,
                        M >= 0, M < 60,
                        S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.


