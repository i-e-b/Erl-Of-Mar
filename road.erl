-module(road).
-compile(export_all).

main() ->
    File = "road.txt",
    {ok, Bin} = file:read_file(File),
    parse_map(Bin).

parse_map(Bin) when is_binary(Bin) ->
    parse_map(binary_to_list(Bin));

parse_map(Str) when is_list(Str) ->
    [list_to_integer(X) || X <- string:tokens(Str, "\r\n\t ")].


