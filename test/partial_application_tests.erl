-module(partial_application_tests).
-compile({parse_transform, partial}).
-include_lib("eunit/include/eunit.hrl").

simple_functionality_test_() ->
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

nested_functionality_test_() ->
    [fun missing_argument_in_nested_function/0,
     fun missing_arguments_in_nesting_and_nested/0].

missing_argument_in_nested_function() ->
    Res = lists:all(erlang:is_atom(_), [a, b, c]),
    ?assert(Res).

missing_arguments_in_nesting_and_nested() ->
    All_atoms = lists:all(erlang:is_atom(_), _),
    ?assert(All_atoms([a, b, c])).

partial_application_of_variable_fun_test_() ->
    [fun partial_application_of_already_partially_applied_function/0].

-record(some_record, {first_field = 1,
                      second_field = 2}).

partial_application_of_already_partially_applied_function() ->
    Is_record = is_record(_, _),
    Is_some_record = Is_record(_, some_record),
    All_are_some_record = lists:all(Is_some_record, _),
    ?assert(All_are_some_record([#some_record{}])).

intersperce([], _) ->
    [];
intersperce([E], _) ->
    [E];
intersperce([E|R], Element) ->
    [E, Element | intersperce(R, Element)].

concat_three_strings(S1, S2, S3) ->
    S1 ++ S2 ++ S3.
