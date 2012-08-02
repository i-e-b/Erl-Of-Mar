-module(dolphin).
-compile(export_all).

dolphin() ->
    receive
        {From, do_a_flip} ->
            From ! "How about a fish instead?";
        {From, fish} ->
            From ! "Thanks for the fish";
        {_, bye} ->
            erlang:exit();
        {From, _} ->
            From ! "I can do_a_flip any time I want"
    end,
    dolphin().

