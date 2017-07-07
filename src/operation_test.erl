-module(operation_test).
-compile({parse_transform, partial}).
-compile(export_all).

add(X, Y) ->
    X + Y.

test() ->
    Increment = _+1,
    lists:map(Increment, [1, 2, 3]).

test2() ->
    lists:map(_/2, [10, 8, 6, 4, 2]).

test3() ->
    Add = _+ f(_+3),
    Add(10).

f(Function) ->
    Function(2).
