-module(sieve).

-export([primes/1, is_prime/1, counter/1, filter/2]).

is_prime(Num, Counter) ->
    case trunc(math:sqrt(Num)) of
        SqrtNum when (Counter > SqrtNum)-> true;
        _ when ((Num rem Counter) == 0) -> false;
        _ -> is_prime(Num, Counter + 1) end.
  

is_prime(Num) when Num < 2 -> false;
is_prime(Num) when (Num =:= 2) or (Num =:= 3) -> true;
is_prime(Num) -> is_prime(Num, 2).


filter(_, []) -> [];
filter(Pred, [H | T]) ->
    case Pred(H) of
        true -> [H | filter(Pred, T)];
        false -> filter(Pred, T) end.

counter(Limit, Limit) -> [Limit]; 
counter(Limit, Counter) -> [Counter | counter(Limit, Counter + 1)].
counter(Limit) when Limit < 2 -> [];
counter(Limit) -> counter(Limit, 2).

primes(_Limit) -> filter(fun(X) -> is_prime(X) end, counter(_Limit)).