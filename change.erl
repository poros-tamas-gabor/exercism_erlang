-module(change).

%in-progress
-export([find_fewest_coins/2, reverse/1, reversed_one_possibility/2, find_min_length/1, find_every_possibility/2]).

reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H];
reverse(_) -> undefined.

concat(undefined, L) -> L; 
concat(_, undefined) -> undefined;
concat(Num, L) -> [Num | L].

reversed_one_possibility(Target, Coins = [C | _]) when Target >= C -> 
    concat(C, reversed_one_possibility(Target - C, Coins));
reversed_one_possibility(Target, [_ | Tail]) -> reversed_one_possibility(Target, Tail);
reversed_one_possibility(0, _) -> [];
reversed_one_possibility(_, []) -> undefined.

find_every_possibility(_, []) -> [];
find_every_possibility(Target, Coins = [_ | Tail]) -> 
    %io:format("one Possibility: ~p\n", [reversed_one_possibility(Target, Coins)]),
    %io:format("coins: ~p\n", [Coins]),
    concat(reversed_one_possibility(Target, Coins), find_every_possibility(Target, Tail)).


find_min_length([]) -> undefined;
find_min_length([L | []]) -> L;
find_min_length([List | Lists]) ->
    case length(find_min_length(Lists)) of
        Min when length(List) < Min -> List;
        _ -> find_min_length(Lists) end.




find_fewest_coins(Target, Coins) -> 
    %io:format("coins: ~p\n", [Coins]),
    %io:format("coins: ~p\n", [reverse(Coins)]),
    reverse(find_min_length(find_every_possibility(Target, reverse(Coins)))).
    