-module(ifs).
-compile(export_all).


wha(X) ->
    if
        X =:= cat -> "cat";
        X =/= cat -> "not cat"
    end.

more_like_it(X) -> case X of
        cat -> "cat";
        _ -> "not cat"
    end.

overloads(cat) -> "cat";
overloads(_) -> "not cat".

