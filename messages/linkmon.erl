-module(linkmon).
-compile(export_all).

fiveSecondExit() ->
    timer:sleep(5000),
    exit(reason).

chain(0) ->
    receive
        _ -> ok
    after 2000 ->
        exit("chain dies here")
    end;
chain(N) ->
    Pid = spawn(fun() -> chain(N-1) end),
    link(Pid),
    receive
        _ -> ok
    end.

trap_exit(N) ->
    process_flag(trap_exit, true),
    spawn_link(fun() -> chain(N) end),
    receive X -> X end.

