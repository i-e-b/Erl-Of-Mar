-module(event).
-compile(export_all).
-record(state, {server, name="", to_go=0}).


start(EventName, Delay) ->
    spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
    spawn_link(?MODULE, init, [self(), EventName, Delay]).

cancel(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, cancel},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    end.


% Innards %
init(Server, EventName, Delay) ->
    loop(#state{
            server=Server,
            name=EventName,
            to_go=Delay}).

loop(S = #state{server=Server}) ->
    Timer = erlang:send_after(S#state.to_go * 1000, Server, {done, S#state.name}),
    receive
        {Server, Ref, cancel} ->
            erlang:cancel_timer(Timer),
            Server ! {Ref, ok}
    end.

% Hack around "after", which isn't the right way to do things! %

