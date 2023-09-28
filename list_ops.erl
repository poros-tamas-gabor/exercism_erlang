-module(list_ops).

-export([append/2, concat/1, filter/2, length/1, map/2, foldl/3, foldr/3,
	 reverse/1]).


append_helper([], List2) -> List2;
append_helper([H | T], List2) -> append_helper(T, [H | List2]).


append(List1, List2) -> append_helper(reverse(List1), List2).

concat(_Lists = [List | Rest], Acc) -> append(List, concat(Rest, Acc));
concat([], Acc) -> reverse(Acc).

concat(List) -> concat(List, []).

filter(_Pred, [], Acc) -> reverse(Acc);
filter(Pred, [H | T], Acc) ->
    case Pred(H) of
        true -> filter(Pred, T, [H | Acc]);
        false -> filter(Pred, T, Acc)
    end.

filter(Function, List) -> filter(Function, List, []).

length([], Acc) -> Acc;
length([_ | T], Acc) -> length(T, Acc + 1).

length(List) -> length(List, 0).

map(Function, _List = [H|T], Acc) -> map(Function, T, [Function(H) | Acc]);
map(_Function, [], Acc) -> reverse(Acc).

map(Function, List) -> map(Function, List, []).

foldl(Function, Acc, [H | T]) -> foldl(Function, Function(H, Acc), T);
foldl(_Function, Acc, []) -> Acc.

foldr(_Function, _Start, _List) -> foldl(_Function, _Start, reverse(_List)).

reverse([], Acc) -> Acc;
reverse([H | T], Acc) -> reverse(T, [H | Acc]).

reverse(List) -> reverse(List, []).
