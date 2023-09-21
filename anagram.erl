-module(anagram).

-export([find_anagrams/2]).
-compile([export_all]).

add(Value, []) -> [{Value, [], []}];
add(Value, [{Value, Left, Right}]) -> [{Value, Left, Right}];
add(Value, [{Node, Left, Right}]) when Value < Node -> [{Node, add(Value, Left), Right}];
add(Value, [{Node, Left, Right}])  -> [{Node, Left, add(Value, Right)}].

contains_key(Key, [{{Key, _}, _Left, _Right}]) -> true;
contains_key(Key, [{{C_Key, C_Value}, Left, _Right}]) when Key < C_Key -> contains_key(Key, Left);
contains_key(Key, [{{C_Key, C_Value}, _Left, Right}]) -> contains_key(Key, Right);
contains_key(Key,[]) -> false.

get_value(Key, [{{Key, Value}, _Left, _Right}]) -> Value;
get_value(Key, [{{C_Key, C_Value}, Left, _Right}]) when Key < C_Key -> get_value(Key, Left);
get_value(Key, [{{C_Key, C_Value}, _Left, Right}]) -> get_value(Key, Right);
get_value(Key,[]) -> error.

set_value({Key, Value}, [{{Key, Old_Value}, Left, Right}]) -> [{{Key, Value}, Left, Right}];
set_value({Key, Value}, [{{C_Key, C_Value}, Left, Right}]) when Key < C_Key -> [{{C_Key, C_Value}, set_value({Key, Value}, Left), Right}];
set_value({Key, Value}, [{{C_Key, C_Value}, Left, Right}]) -> [{{C_Key, C_Value},  Left, set_value({Key, Value}, Right)}].

tree_to_list([]) -> [];
tree_to_list({Value, [], T}) -> [Value | tree_to_list(T)];
tree_to_list([{Value, Left, Right}]) -> tree_to_list(Left) ++ [Value] ++ tree_to_list(Right).

string_to_tree([]) -> [];
string_to_tree([H | T]) ->
    case contains_key(H, string_to_tree(T)) of
        true -> 
            OldValue = get_value(H, string_to_tree(T)),
            set_value({H, OldValue + 1}, string_to_tree(T));
        false -> add({H, 1}, string_to_tree(T)) end.

is_anagram(Str, Str) -> false;
is_anagram(Str1, Str2) -> 
    T1 = string_to_tree(Str1),
    T2 = string_to_tree(Str2),
    L1 = tree_to_list(T1),
    L2 = tree_to_list(T2),
    L1 -- L2 =:= L2 -- L1.

find_anagrams(Subject, Candidates) -> 
    lists:filter(fun(Word) -> is_anagram(string:lowercase(Subject), string:lowercase(Word)) end,  Candidates).
