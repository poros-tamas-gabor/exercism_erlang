-module(perfect_numbers).

-export([classify/1]).

get_factors(Number, Number) -> [];
get_factors(Number, Factor) when Number rem Factor =:= 0 -> [Factor | get_factors(Number, Factor + 1)];
get_factors(Number, Factor) -> get_factors(Number, Factor + 1).

get_factors(0) -> error;
get_factors(Number) -> get_factors(Number, 1).


classify(Number) when Number < 1 -> erlang:error(badarg);
classify(Number) -> 
    case lists:sum(get_factors(Number)) of 
        Sum when Sum =:= Number -> perfect;
        Sum when Sum < Number -> deficient;
        Sum when Sum > Number -> abundant end.

