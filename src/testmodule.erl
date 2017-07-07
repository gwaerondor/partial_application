-module(testmodule).

-compile({parse_transform, partial}).
-compile(export_all).

hello() ->
    Integers = [1, 2, 3, 4, 5],
    Doubles = lists:map(multiply(_, 2), Integers),
    io:format("Doubles of ~p: ~p~n", [Integers, Doubles]),
    Tag = wrap("<", _, ">"),
    Tag("html").
                      
add(X, Y) ->
    X + Y.

multiply(X, Y) ->
    X * Y.

wrap(Start, Middle, End) ->
    Start ++ Middle ++ End.
