-module(functions).
-compile(export_all).

head([H|_]) -> H.
second([_,X|_]) -> X.
