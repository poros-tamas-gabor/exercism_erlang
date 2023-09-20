-module(set).

-compile([export_all]).
%-export().


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
