-module(palindrome_products).

-export([largest/2, smallest/2]).


add(Value, []) -> [{Value, [], []}];
add(Value, [{Value, Left, Right}]) -> [{Value, Left, Right}];
add(Value, [{Node, Left, Right}]) when Value < Node -> [{Node, add(Value, Left), Right}];
add(Value, [{Node, Left, Right}])  -> [{Node, Left, add(Value, Right)}].

contains(Value, [{Value, _Left, _Right}]) -> true;
contains(_, []) -> false;
contains(Value, [{Node, _Left, _Right}]) when Value < Node -> contains(Value, _Left);
contains(Value, [{_Node, _Left, _Right}]) -> contains(Value, _Right).

tree_to_list([]) -> [];
tree_to_list({Value, [], T}) -> [Value | tree_to_list(T)];
tree_to_list([{Value, Left, Right}]) -> tree_to_list(Left) ++ [Value] ++ tree_to_list(Right).


first_digit([]) -> [];
first_digit([H | _]) -> H.

remove_first_digit([]) -> [];
remove_first_digit([_ | T]) -> T. 

last_digit(Num) -> first_digit(reverse(Num)).

remove_last_digit(Num) -> reverse(remove_first_digit(reverse(Num))).

remove_last_first_digit(Num) -> remove_last_digit(remove_first_digit(Num)).

is_palindrome(Num) when is_integer(Num) -> is_palindrome(integer_to_list(Num));
is_palindrome(Num) when length(Num) =< 1 -> true;
is_palindrome(Num) ->
    case first_digit(Num) =:= last_digit(Num) of
        true -> is_palindrome(remove_last_first_digit(Num));
        false -> false end.


filter(_Pred, []) -> [];
filter(Pred, [H  | T]) -> 
    case Pred(H) of
        true -> [H | filter(Pred, T)];
        false -> filter(Pred, T) end.

reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].

list_of_palindrome_in_range2_helper(_Min, Max, CurrentPalindrom) when CurrentPalindrom > Max -> [];
list_of_palindrome_in_range2_helper(Min, Max, CurrentPalindrom) -> [CurrentPalindrom | list_of_palindrome_in_range2_helper(Min, Max,gen_next_palindrome(CurrentPalindrom))].
find_first_palindrom(Min) ->
    case is_palindrome(Min) of
        true -> Min;
        false -> find_first_palindrom(Min + 1) end.

list_of_palindrome_in_range2(Min, Max) -> list_of_palindrome_in_range2_helper(Min, Max, find_first_palindrom(Min)).
    
gen_next_palindrome_increase_digit_num(_, 0) -> "1";
gen_next_palindrome_increase_digit_num(Count, Count) -> "1" ++ gen_next_palindrome_increase_digit_num(Count, Count - 1);
gen_next_palindrome_increase_digit_num(OrginalCount, Count) -> "0" ++ gen_next_palindrome_increase_digit_num(OrginalCount, Count - 1).

gen_next_palindrome_increase_digit_num(LogNum) -> list_to_integer(gen_next_palindrome_increase_digit_num(LogNum, LogNum)).

get_half([H | _T], Count, Length) when (Length rem 2 =:= 0) and (Count =:= Length div 2 - 1) -> [H];
get_half([H | T], Count, Length) when (Length rem 2 =:= 0) and (Count < Length div 2 - 1) -> [H | get_half(T, Count + 1, Length)];

get_half([H | _T], Count, Length) when (Length rem 2 =/= 0) and (Count =:= Length div 2) -> [H];
get_half([H | T], Count, Length) when (Length rem 2 =/= 0) and (Count < Length div 2) -> [H | get_half(T, Count + 1, Length)].

get_half(StrNum) -> get_half(StrNum, 0, length(StrNum)).

gen_next_palindrome_helper(StrNum) when is_list(StrNum) ->
    case length(StrNum) rem 2 =:= 0 of
        true -> 
            HalfStr = integer_to_list(list_to_integer(get_half(StrNum)) + 1),
            list_to_integer(HalfStr ++ reverse(HalfStr));
        false -> 
            HalfStr = integer_to_list(list_to_integer(get_half(StrNum)) + 1),
            list_to_integer(remove_last_digit(HalfStr) ++ [last_digit(HalfStr)] ++ reverse(remove_last_digit(HalfStr)))
    end.

gen_next_palindrome(Num) when is_integer(Num), Num < 9 -> Num + 1;
gen_next_palindrome(Num) when is_integer(Num) ->
    case math:log10(Num + 1) ==  trunc(math:log10(Num + 1)) of
        true -> gen_next_palindrome_increase_digit_num(trunc(math:log10(Num + 1)));
        false -> gen_next_palindrome_helper(integer_to_list(Num)) end.

find_factors_for_num_in_range(_Min, Max, Counter, _Num) when Counter > Max -> [];
find_factors_for_num_in_range(Min, Max, Counter, Num) ->
    case Num rem Counter =:= 0 of
        true -> 
            if Num div Counter >= Min, Num div Counter =< Max -> add({min(Counter, Num div Counter), max(Counter, Num div Counter)}, find_factors_for_num_in_range(Min, Max, Counter + 1, Num));
            true -> find_factors_for_num_in_range(Min, Max, Counter + 1, Num) end;
        false -> find_factors_for_num_in_range(Min, Max, Counter + 1, Num) end.


helper(_Min, _Max, []) -> [];
helper(_Min, _Max, _Palindromes = [H | Tail]) -> 
    Factors = find_factors_for_num_in_range(_Min, _Max, _Min, H),
    if 
        Factors =:= [] -> helper(_Min, _Max, Tail);
        Factors =/= [] -> 
            List = tree_to_list(Factors),
            [{N1, N2} | _ ] = List,
            {N1*N2, List}
    end.


largest(Min, Max) when Min > Max -> erlang:error(badarg);
largest(Min, Max) -> 
    Palindromes = reverse(list_of_palindrome_in_range2(Min*Min, Max*Max)),
    List = helper(Min, Max, Palindromes),
    case List =:= [] of
        true -> undefined;
        false -> List 
    end.

smallest(Min, Max) when Min > Max -> erlang:error(badarg);
smallest(Min, Max) -> 
    Palindromes = list_of_palindrome_in_range2(Min*Min, Max*Max),
    List = helper(Min, Max, Palindromes),
    case List =:= [] of
        true -> undefined;
        false -> List 
    end.
