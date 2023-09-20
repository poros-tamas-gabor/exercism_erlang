-module(custom_set).

-export([add/2, contains/2, difference/2, disjoint/2, empty/1, equal/2, from_list/1, intersection/2, subset/2,
	 union/2]).

contains(_, []) -> false;
contains(Elem, [Elem | _]) -> true;
contains(Elem, [_ | Tail]) -> contains(Elem, Tail).

is_same_length([], []) -> true;
is_same_length([_ | T1], [_ | T2]) -> is_same_length(T1, T2);
is_same_length(_, _) -> false.

add(Elem, []) -> [Elem];
add(Elem, Set) ->
    case contains(Elem, Set) of 
        true -> Set;
        false -> [Elem | Set] end.

remove(_, []) -> [];
remove(Elem, [Elem | T]) -> remove(Elem, T);
remove(Elem, [H | T]) -> [H | remove(Elem, T)].

difference(Set, []) -> Set;
difference(Set, [H | T]) ->
    case contains(H, Set) of
        true -> difference(remove(H, Set), T);
        false -> difference(Set, T) end.

disjoint([H | T], Set) -> 
    case contains(H, Set) of
        true -> false;
        false -> disjoint(T, Set) end;
disjoint([], _) -> true.

empty([]) -> true;
empty(_) -> false.

subset([], _) -> true;
subset([H | T], Set2) -> 
    case contains(H, Set2) of
        true -> subset(T, Set2);
        false -> false end.

equal(Set1, Set2) -> 
    case is_same_length(Set1, Set2) of
        true -> subset(Set1, Set2);
        false -> false end.

from_list([]) -> [];
from_list([H | T]) -> add(H, from_list(T)).

intersection([], Set) -> [];
intersection([H | T], Set) ->
    case contains(H, Set) of
        true -> [H | intersection(T, Set)];
        false -> intersection(T, Set) end.   


union(Set, []) -> Set;
union(Set, [H | T]) -> add(H, union(Set, T)).
