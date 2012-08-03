-module(linkmon).
-compile(export_all).

fiveSecondExit() ->
    timer:sleep(5000),
    exit(reason).

