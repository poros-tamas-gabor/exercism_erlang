-module(roman_numerals).

-export([roman/1]).
roman_letter(1000) -> $M;
roman_letter(500) -> $D;
roman_letter(100) -> $C;
roman_letter(50) -> $L;
roman_letter(10) -> $X;
roman_letter(5) -> $V;
roman_letter(1) -> $I.


printN(_, Num) when Num < 0 -> printN(0 , 0);
printN(_, 0) -> "";
printN(Char, Num) -> [Char] ++ printN(Char, Num - 1).

get_roman_letter_by_digit_log(Digit, _Lognum) ->
    if
        Digit < 4 ->  printN( roman_letter( round(math:pow(10, _Lognum))), Digit);
        Digit > 3, Digit < 9 -> printN( roman_letter( round(math:pow(10, _Lognum))), 5 - Digit) ++ [roman_letter(5 * round(math:pow(10, _Lognum)))] 
            ++ printN( roman_letter( round(math:pow(10, _Lognum))), Digit - 5);
        Digit =:= 9 -> [roman_letter( round(math:pow(10, _Lognum)))] ++ [roman_letter(round(math:pow(10, _Lognum + 1)))]
    end.

first_digit(Num) ->
    trunc(Num / math:pow(10,trunc(math:log10(Num)))).

remove_first_digit(Num) ->
    Num rem trunc(math:pow(10,trunc(math:log10(Num)))).


roman(0) -> "";
roman(Num) ->
    Digit = first_digit(Num),
    LogNum = trunc(math:log10(Num)),
    get_roman_letter_by_digit_log(Digit, LogNum) ++ roman( remove_first_digit(Num) ).
