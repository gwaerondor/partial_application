-module(partial_application_tests).
-compile({parse_transform, partial}).
-include_lib("eunit/include/eunit.hrl").

core_functionality_test_() ->
    [fun arity_two_function_with_second_argument_missing/0,
     fun arity_two_function_with_first_argument_missing/0,
     fun arity_three_function_with_two_arguments_missing/0,
     fun wrapped_in_parentheses/0,
     fun apply_argument_to_partially_applied_function_in_parentheses/0,
     fun apply_argument_to_partially_applied_function/0,
     fun return_partially_applied_fun_from_code_block/0,
     fun apply_argument_to_nested_variable_function/0,
     fun apply_argument_to_partially_applied_fun_in_code_block/0,
     fun missing_argument_in_nested_function/0,
     fun missing_arguments_in_nesting_and_nested/0,
     fun partial_application_of_already_partially_applied_function/0
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

wrapped_in_parentheses() ->
    Is_integer = (is_integer(_)),
    ?assert(Is_integer(1)).

apply_argument_to_partially_applied_function_in_parentheses() ->
    One_is_integer = (is_integer(_))(1),
    ?assert(One_is_integer).

apply_argument_to_partially_applied_function() ->
    Two_is_integer = is_integer(_)(2),
    ?assert(Two_is_integer).

return_partially_applied_fun_from_code_block() ->
    Are_integers = begin
                       Is_integer = is_integer(_),
                       lists:all(Is_integer, _)
                   end,
    ?assert(Are_integers([1, 2, 3])).

apply_argument_to_nested_variable_function() ->
    Double = fun(X) -> X * 2 end,
    Triple = fun(X) -> X * 3 end,
    Result = Double(Triple(_)(1))
    ?assertEqual(6, Double(Triple(_))(1)).

apply_argument_to_partially_applied_fun_in_code_block() ->
    Result = begin math:log10(_) end(10),
    ?assertEqual(1.0, Result).

missing_argument_in_nested_function() ->
    Res = lists:all(erlang:is_atom(_), [a, b, c]),
    ?assert(Res).

missing_arguments_in_nesting_and_nested() ->
    All_atoms = lists:all(erlang:is_atom(_), _),
    ?assert(All_atoms([a, b, c])).

-record(some_record, {first_field = 1,
                      second_field = 2}).

partial_application_of_already_partially_applied_function() ->
    Is_record = is_record(_, _),
    Is_some_record = Is_record(_, some_record),
    All_are_some_record = lists:all(Is_some_record, _),
    ?assert(All_are_some_record([#some_record{}])).

partial_application_inside_assert_macro() ->
    ?assert(lists:all(is_integer(_), [1, 2, 3])).

partial_application_inside_assert_match_macro() ->
    Keyfind = lists:keyfind(_, 1, _),
    Properties = [{key, value}, {other_key, other_value}],
    ?assertMatch({key, _}, lists:keyfind(_, 1, _)(key, Properties)).

partial_application_inside_assert_equal_macro() ->
    ?assertEqual(100, lists:max(_)([1, 100, 10])).

intersperce([], _) ->
    [];
intersperce([E], _) ->
    [E];
intersperce([E|R], Element) ->
    [E, Element | intersperce(R, Element)].

concat_three_strings(S1, S2, S3) ->
    S1 ++ S2 ++ S3.
