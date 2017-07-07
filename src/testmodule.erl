-module(testmodule).

-compile({parse_transform, partial}).
-export([hello/0]).

hello() ->
    Double = multiply(_, 2),
    lists:map(Double, [1, 2, 3, 4]),
    Tag = wrap("<", _, ">"),
    Tag("html").
                      
add(X, Y) ->
    X + Y.

multiply(X, Y) ->
    X * Y.

wrap(Start, Middle, End) ->
    Start ++ Middle ++ End.
