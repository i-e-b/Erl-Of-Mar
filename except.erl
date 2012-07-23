-module(except).
-compile(export_all).

catcher(X,Y) ->
	case catch X/Y of
		{'EXIT', {badarith, _}} -> 0;
		N->N
	end.

