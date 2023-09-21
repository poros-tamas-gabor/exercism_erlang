-module(map).
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