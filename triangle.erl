-module(triangle).

-export([kind/3, sort3/3]).

get_middle(A, B, C, A, B) -> C;
get_middle(A, B, C, B, A) -> C;

get_middle(A, B, C, B, C) -> A;
get_middle(A, B, C, C, B) -> A;

get_middle(A, B, C, A, C) -> B;
get_middle(A, B, C, C, A) -> B.

sort3(A, B, C) -> { min(min(A,B),C) ,  get_middle(A,B,C, min(min(A,B),C), max(max(A,B),C)) , max(max(A,B),C) }.


kind({0,_,_}) -> { error , "all side lengths must be positive" };
kind({_,0,_}) -> { error , "all side lengths must be positive" };
kind({_,_,0}) -> { error , "all side lengths must be positive" };
kind({A,B,C}) when A + B =< C -> { error , "side lengths violate triangle inequality" };
kind({_A,_A,_A})  -> equilateral;
kind({_,_B,_B})  -> isosceles;
kind({_A,_A,_})  -> isosceles;
kind({_,_,_})  -> scalene.


kind(A, B, C) -> kind(sort3(A,B,C)).
