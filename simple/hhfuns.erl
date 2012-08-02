-module(hhfuns).
-export([map/2, even/1, filter/2]).

one() -> 1.
two() -> 2.

add(X,Y) -> X() + Y().

map(_, []) ->[];
map(F,[H|T])->[F(H)|map(F,T)].

incr(X) -> X + 1.
decr(X) -> X - 1.

incrl(List) -> map(fun incr/1, List).
decrl(List) -> map(fun decr/1, List).

even(L) -> even(L, []).
even([], Acc) -> Acc;
even([H|T], Acc) when H rem 2 == 0 ->
    even(T, Acc ++ [H]);
even([_|T], Acc) ->
    even(T, Acc).

filter(Pred, List) -> filter(Pred, List, []).
filter(_,[],Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
    case Pred(H) of
        true -> filter(Pred, T, Acc ++ [H]);
        false -> filter(Pred, T, Acc)
    end.

