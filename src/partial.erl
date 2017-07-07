-module(partial).
-export([parse_transform/2]).
-define(WILDCARDED_ARGUMENT, {var, _, '_'}).

parse_transform([File, Module, Exports | Abstract], _) ->
    io:format("Original AST: ~p~n", [Abstract]),
    Transformed = transform(Abstract),
    io:format("Transformed AST: ~p~n", [Transformed]),
    [File, Module, Exports | Transformed].

transform({op, Line, Name, Left, Right}) ->
    case lists:any(fun is_wildcard_variable/1, [Left, Right]) of
	true ->
	    generate_partially_applied_operation(Line, Name, transform(Left), transform(Right));
	false ->
	    {op, Line, Name, transform(Left), transform(Right)}
    end;
transform({call, Line, Name, Args}) ->
    case lists:any(fun is_wildcard_variable/1, Args) of
        true ->
            generate_partially_applied_function(Line, Name, transform(Args));
        false ->
            {call, Line, Name, transform(Args)}
    end;
transform(Xs) when is_tuple(Xs) ->
    list_to_tuple(transform(tuple_to_list(Xs)));
transform([X|Xs]) when is_list(Xs) ->
    [transform(X) | transform(Xs)];
transform(X) ->
    X.

is_wildcard_variable(?WILDCARDED_ARGUMENT) ->
    true;
is_wildcard_variable(_) ->
    false.

generate_partially_applied_operation(Line, Name, Left, Right) ->
    New_left = case Left of
		   ?WILDCARDED_ARGUMENT ->
		       {var, Line, create_unapplied_argument_name(1)};
		   _ ->
		       transform(Left)
	       end,
    New_right = case Right of
		    ?WILDCARDED_ARGUMENT ->
			{var, Line, create_unapplied_argument_name(2)};
		    _ ->
			transform(Right)
		end,
    Missing_argument_indices = find_wildcarded_indices([Left, Right]),
    Fun_args = generate_fun_arguments(Missing_argument_indices, Line),
    Fun_body = [{op, Line, Name, New_left, New_right}],
    {'fun', Line, {clauses, [{clause, Line, Fun_args, [], Fun_body}]}}.

generate_partially_applied_function(Line, Name, Args) ->
    Missing_argument_indices = find_wildcarded_indices(Args),
    Fun_args = generate_fun_arguments(Missing_argument_indices, Line),
    Fun_body = replace_wildcarded_arguments(Line, Name, Missing_argument_indices, Args),
    {'fun', Line, {clauses, [{clause, Line, Fun_args, [], Fun_body}]}}.

find_wildcarded_indices(Args) ->
    find_wildcarded_indices(Args, 1).

find_wildcarded_indices([], _) ->
    [];
find_wildcarded_indices([?WILDCARDED_ARGUMENT | Rest], N) ->
    [N | find_wildcarded_indices(Rest, N + 1)];
find_wildcarded_indices([_ | Rest], N) ->
    find_wildcarded_indices(Rest, N + 1).

generate_fun_arguments([], _) ->
    [];
generate_fun_arguments([Index | Indices], Line) ->
    Argument_name = create_unapplied_argument_name(Index),
    [{var, Line, Argument_name} | generate_fun_arguments(Indices, Line)].

create_unapplied_argument_name(Index) ->
    Name = "Unapplied_argument_" ++ integer_to_list(Index),
    list_to_atom(Name).

replace_wildcarded_arguments(Line, Name, Missing_argument_indices, Args) ->
    Call_args = replace_indices_with_unapplied_arguments(Missing_argument_indices, Args, Line),
    [{call, Line, Name, Call_args}].

replace_indices_with_unapplied_arguments([], Args, _) ->
    Args;
replace_indices_with_unapplied_arguments([Index | Indices], Args, Line) ->
    New_args = replace_nth(Index, Args, {var, Line, create_unapplied_argument_name(Index)}),
    replace_indices_with_unapplied_arguments(Indices, New_args, Line).

replace_nth(1, [_ | T], Replacement) ->
    [Replacement | T];
replace_nth(N, [H | T], Replacement) ->
    [H | replace_nth(N - 1, T, Replacement)].
