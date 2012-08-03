-module(multiproc).
-compile(export_all).

flush_important_first() ->
    receive
        {Priority, Message} when Priority > 10 ->
            [Message | flush_important_first()]
    after 0 ->
        normal()
    end.

normal() ->
    receive
        {_, Message} ->
            [Message | normal()]
    after 0 ->
        []
    end.


