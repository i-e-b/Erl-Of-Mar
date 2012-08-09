-module(kitty_gen_server).
-behaviour(gen_server).


-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(cat, {name, color=green, description}).

% API %
start_link() -> gen_server:start_link(?MODULE, [], []).

order_cat(Pid, Name, Color, Description) ->
    gen_server:call(Pid, {order, Name, Color, Description}). %sync call

return_cat(Pid, Cat = #cat{}) ->
    gen_server:cast(Pid, {return, Cat}). % async

close_shop(Pid) ->
    gen_server:call(Pid, terminate).

% Server %
init([]) -> {ok, []}.

handle_call({order, Name, Color, Description}, _From, Cats) ->
    if
        Cats =:= [] ->
            {reply, make_cat(Name, Color, Description), Cats};
        Cats =/= [] ->
            {reply, hd(Cats), tl(Cats)}
    end;
handle_call(terminate, _From, Cats) ->
    {stop, normal, ok, Cats}.

handle_cast({return, Cat = #cat{}}, Cats) ->
    {noreply, [Cat|Cats]}.

handle_info(Msg, Cats) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, Cats}.

terminate(normal, Cats) ->
    [io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. % Original version, so never used -- next version's will be?

% Implementations %
make_cat(Name, Color, Description) ->
    #cat{name=Name, color=Color, description=Description}.
