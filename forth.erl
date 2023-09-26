-module(forth).

-export([evaluate/1]).



evaluate([], {Stack, Vars}) -> {Stack, Vars}; 
evaluate([Row | Rows], {Stack, Vars}) when is_list(Row) -> evaluate(Rows, evaluate(Row, {Stack, Vars}));
evaluate(Row = [Char | _Chars], {Stack, Vars}) when is_integer(Char) -> evaluate_tokens(lists:map(fun(Word) -> string:lowercase(Word) end, string:tokens(Row, " ")), {Stack, Vars}).


evaluate_tokens([], {Stack, Vars}) -> {Stack, Vars};

evaluate_tokens(_Row = [":" | Tokens], {Stack, Vars}) -> create_variable(Tokens, {Stack, Vars});

evaluate_tokens([Token | Tokens], {Stack, Vars}) -> 
    case contains(Token, Vars) of
        true -> evaluate_tokens(get_key(Token, Vars) ++ Tokens, {Stack, Vars});
        false -> evaluate_tokens_not_vars([Token | Tokens], {Stack, Vars}) end.



contains(_Token, []) -> false;
contains(Token, [{Token, _Value} | _T]) -> true;
contains(Token, [{_Key, _Value} | T]) -> contains(Token, T).

get_key(Token, [{Token, Value} | _T]) -> Value;
get_key(Token, [{_, _} | T]) -> get_key(Token, T).

evaluate_tokens_not_vars(["drop" | Tokens], {_Stack = [_H | T], Vars}) ->  evaluate_tokens(Tokens, {T, Vars});
evaluate_tokens_not_vars(["drop" | _Tokens], {_Stack = [], _Vars}) ->  erlang:error(badarg);

evaluate_tokens_not_vars(["dup" | Tokens], {Stack = [H | _T], Vars}) ->  evaluate_tokens(Tokens, {[H | Stack], Vars});
evaluate_tokens_not_vars(["dup" | _Tokens], {[], _Vars}) ->  erlang:error(badarg);

evaluate_tokens_not_vars(["swap" | Tokens], {_Stack = [H1 , H2 | T], Vars}) ->  evaluate_tokens(Tokens, {[H2 , H1 | T], Vars});
evaluate_tokens_not_vars(["swap" | _Tokens], {_, _Vars}) ->  erlang:error(badarg);

evaluate_tokens_not_vars(["over" | Tokens], {_Stack = [H1 , H2 | T], Vars}) ->  evaluate_tokens(Tokens, {[H2 , H1, H2 | T], Vars});
evaluate_tokens_not_vars(["over" | _Tokens], {_, _Vars}) ->  erlang:error(badarg);

evaluate_tokens_not_vars(["+" | Tokens], {_Stack = [H1 , H2 | T], Vars}) ->  evaluate_tokens(Tokens, {[H2 + H1 | T], Vars});
evaluate_tokens_not_vars(["+" | _Tokens], {_, _Vars}) ->  erlang:error(badarg);

evaluate_tokens_not_vars(["*" | Tokens], {_Stack = [H1 , H2 | T], Vars}) ->  evaluate_tokens(Tokens, {[H2 * H1 | T], Vars});
evaluate_tokens_not_vars(["*" | _Tokens], {_, _Vars}) ->  erlang:error(badarg);

evaluate_tokens_not_vars(["-" | Tokens], {_Stack = [H1 , H2 | T], Vars}) ->  evaluate_tokens(Tokens, {[H2 - H1 | T], Vars});
evaluate_tokens_not_vars(["-" | _Tokens], {_, _Vars}) ->  erlang:error(badarg);

evaluate_tokens_not_vars(["/" | Tokens], {_Stack = [H1 , H2 | T], Vars}) ->  evaluate_tokens(Tokens, {[H2 div H1 | T], Vars});
evaluate_tokens_not_vars(["/" | _Tokens], {_, _Vars}) ->  erlang:error(badarg);

evaluate_tokens_not_vars([Num | Tokens], {Stack, Vars}) ->  evaluate_tokens(Tokens, {[list_to_integer(Num) | Stack], Vars});
evaluate_tokens_not_vars([], {Stack, Vars}) -> {Stack, Vars}.


evaluate(Instruction) -> lists:reverse(element(1,evaluate(Instruction, {[],[]}))).


create_variable([Var_Name | Tokens], {Stack, Vars}) -> 
    case is_numeric(Var_Name) of
        true -> erlang:error(badarg);
        false ->
            evaluate_tokens_for_vars(Tokens, {Stack, [{Var_Name , ""} | Vars]}) end.


is_numeric(Str) -> 
    case re:run(Str, "[-]?[0-9]+") of
        {match,[{0, Num}]} when Num =:= length(Str) -> true;
        _ -> false end.

evaluate_tokens_for_vars([";" | []], {Stack, Vars}) -> 
    io:format("evaluate_tokens_for_vars ~p ~p\n", [Stack, Vars]),
    {Stack, Vars};
evaluate_tokens_for_vars([Token | Tokens], {Stack, [{Var_Name, Value} | Vars]}) ->
    case contains(Token, Vars) of
        true -> 
            TokenValue = get_key(Token, Vars),
            evaluate_tokens_for_vars(TokenValue ++ Tokens, {Stack, [{Var_Name, Value} | Vars]});
        false -> evaluate_tokens_for_vars(Tokens, {Stack, [{Var_Name, Value ++ [Token]} | Vars]}) end.


