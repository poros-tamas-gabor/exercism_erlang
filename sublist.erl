-module(sublist).

-export([is_equal/2, is_sublist/2, is_superlist/2, is_unequal/2, relation/2]).


is_equal([], []) -> true;
is_equal([H | T1], [H | T2]) -> is_equal(T1, T2);
is_equal([_H1 | _], [_H2 | _]) -> false;
is_equal(_, _) -> false.

is_sublist(L1, L2) -> 
    case length(L1) =< length(L2) of
        true -> is_sublist(L1, L2, []);
        false -> false end.


is_sublist([], _, _) -> true;
is_sublist(_L1, [], _) -> false;
is_sublist(_L1 = [H | T1], _L2 = [H | T2], Acc) -> is_sublist(T1, T2, [H | Acc]);
is_sublist(L1 = [H | T1], L2 = [_H | T2], []) -> is_sublist(L1, T2, []);
is_sublist(L1 = [H | T1], L2 = [_H | T2], Acc) -> is_sublist(lists:reverse(Acc) ++ L1, tl(lists:reverse(Acc) ++ L2), []).

is_superlist(L1, L2) -> is_sublist(L2, L1).

is_unequal(L1, L2) -> not is_equal(L1, L2). %and not is_sublist(L1,L2) and not is_superlist(L1, L2).

relation(L1, L2) -> 
    case is_equal(L1, L2) of
        true -> equal;
        false -> 
            case is_sublist(L1, L2) of
                true -> sublist;
                false ->
                    case is_superlist(L1, L2) of
                        true -> superlist;
                        false -> unequal end end end.