-module(allergies).

-export([allergies/1, is_allergic_to/2]).

%eggs (1)
%peanuts (2)
%shellfish (4)
%strawberries (8)
%tomatoes (16)
%chocolate (32)
%pollen (64)
%cats (128)
add_allergy(1, Acc) -> [eggs | Acc];
add_allergy(2, Acc) -> [peanuts | Acc];
add_allergy(4, Acc) -> [shellfish | Acc];
add_allergy(8, Acc) -> [strawberries | Acc];
add_allergy(16, Acc) -> [tomatoes | Acc];
add_allergy(32, Acc) -> [chocolate | Acc];
add_allergy(64, Acc) -> [pollen | Acc];
add_allergy(128, Acc) -> [cats | Acc];
add_allergy(_, Acc) -> Acc.

allergies(0, Acc) -> Acc;
allergies(Score, Acc) -> 
    GreatestTwoAdN = trunc(math:pow(2,trunc(math:log2(Score)))),
    %io:format("Score ~p, 2^n: ~p, Acc: ~p\n", [Score, GreatestTwoAdN, Acc]),
    allergies(Score - GreatestTwoAdN, add_allergy(GreatestTwoAdN, Acc)).

allergies(Score) -> allergies(Score, []).

contains([], _) -> false;
contains([H | _], H) -> true;
contains([_ | T], H) -> contains(T,H).

is_allergic_to(Substance, Score) -> contains(allergies(Score), Substance).
