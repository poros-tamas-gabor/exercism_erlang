-module(sum_of_multiples).

-export([sum/2]).
-compile([export_all]).
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

list_to_tree([]) -> [];
list_to_tree([H | T]) -> add(H, list_to_tree(T)).

find_multiples_for_factor(0,_,_) -> [];
find_multiples_for_factor(_Factor, Current, Max) when Current > Max -> [];
find_multiples_for_factor(Factor, Current, Current) when Factor =< Current -> [];
find_multiples_for_factor(Factor, Current, Max) -> [Current | find_multiples_for_factor(Factor, Current + Factor, Max)].

for_loop_factors([], _Limit) -> []; 
for_loop_factors([Factor | Factors], Limit) -> [find_multiples_for_factor(Factor, Factor, Limit) | for_loop_factors(Factors, Limit)].


sum(Factors, Limit) -> 
    Multiples = lists:flatten(for_loop_factors(Factors, Limit)),
    SetOfMultiples = tree_to_list(list_to_tree(Multiples)),
    lists:sum(SetOfMultiples).


