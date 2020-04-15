-module(partial_application_tests).
-compile({parse_transform, partial}).
-include_lib("eunit/include/eunit.hrl").

compilation_and_execution_test_() ->
    [fun arity_two_function_with_second_argument_missing/0,
     fun arity_two_function_with_first_argument_missing/0,
     fun arity_three_function_with_two_arguments_missing/0
    ].

arity_two_function_with_second_argument_missing() ->
    Count_to = lists:seq(1, _),
    Expected = lists:seq(1, 5),
    Result = Count_to(5),
    ?assertEqual(Expected, Result).

arity_two_function_with_first_argument_missing() ->
    Spell_out = intersperce(_, $-),
    Expected = "H-e-l-l-o",
    Result = Spell_out("Hello"),
    ?assertEqual(Expected, Result).

arity_three_function_with_two_arguments_missing() ->
    Make_header = concat_three_strings("<h1>", _, "</h1>"),
    Expected = "<h1>Chapter 1</h1>",
    Result = Make_header("Chapter 1"),
    ?assertEqual(Expected, Result).

intersperce([], _) ->
    [];
intersperce([E], _) ->
    [E];
intersperce([E|R], Element) ->
    [E, Element | intersperce(R, Element)].

concat_three_strings(S1, S2, S3) ->
    S1 ++ S2 ++ S3.
