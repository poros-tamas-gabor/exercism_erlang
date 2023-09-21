-module(armstrong_numbers).

-export([is_armstrong_number/1]).

-compile([export_all]).
get_number_of_digit(0) -> 1;
get_number_of_digit(Num) when is_integer(Num) -> trunc(math:log10(Num)) + 1.

number_to_digits(Num) when Num < 10 -> [Num];
number_to_digits(Num) -> 
    Log10 = get_number_of_digit(Num) - 1,
    FstDigit = Num div round(math:pow(10, Log10)),
    RemDigits = Num rem round(math:pow(10, Log10)),
    [FstDigit | number_to_digits(RemDigits)].

is_armstrong_number(Number) -> lists:sum(lists:map(fun(X) -> round(math:pow(X,get_number_of_digit(Number))) end, number_to_digits(Number))) =:= Number.
