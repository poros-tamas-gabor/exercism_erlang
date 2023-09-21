-module(pythagorean_triplet).

-export([triplets_with_sum/1]).

-compile([export_all]).

%https://en.wikipedia.org/wiki/Pythagorean_triple
%a=m^{2}-n^{2},\ \,b=2mn,\ \,c=m^{2}+n^{2}

factors(Num) -> factors(Num, 1).
factors(Num, Num) -> [Num]; 
factors(Num, Count) when Num rem Count =:= 0 -> [Count | factors(Num, Count + 1)];
factors(Num, Count) -> factors(Num, Count + 1).

find_m_n(Limit) when Limit rem 2 =:= 1 -> [];
find_m_n(Limit) -> 
    LimitHalf = Limit div 2,
    Ms = lists:filter( fun(X) -> ((LimitHalf div X) - X < X) and (X < LimitHalf div X) end , factors(LimitHalf)),
    lists:map(fun(M) -> {M, LimitHalf div M - M} end, Ms).

calculate_a({M, N}) -> trunc(math:pow(M,2) - math:pow(N, 2)).
calculate_b({M, N}) -> 2*M*N.
calculate_c({M, N}) -> trunc(math:pow(M,2) + math:pow(N, 2)).

calculate_a_b_c({M,N}) ->     
    A = calculate_a({M, N}),
    B = calculate_b({M, N}),
    C = calculate_c({M, N}),
    {min(A,B), max(A,B), C}.

find_similar_triangle({A,B,C}, Perimeter) when A + B + C =:= Perimeter -> {A,B,C};
find_similar_triangle({A,B,C}, Perimeter) -> 
    K = Perimeter div (A + B + C),
    {K*A, K*B, K*C}.

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

triplets_with_sum(Limit) when Limit rem 2 =:= 1 -> [];
triplets_with_sum(Limit) when Limit rem 2 =:= 0 -> 
    Factors = factors(Limit),
    MNList = lists:flatten(lists:map(fun(Factor) -> find_m_n(Factor) end, Factors)),
    Triangles = lists:map(fun(M_N) -> calculate_a_b_c(M_N) end, MNList),
    tree_to_list(list_to_tree(lists:map(fun(Tri) -> find_similar_triangle(Tri, Limit) end, Triangles))).

