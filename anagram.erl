-module(anagram).

-export([find_anagrams/2]).
-compile([export_all]).


change_first_char(_Char, []) -> [];
change_first_char(Char, [Char | T]) -> [$A | T];
change_first_char(Char, [H | T]) -> [H | change_first_char(Char, T)].
%just for lowercase words
change_same_chars([], Str2) -> Str2;
change_same_chars([H | T], Str2) -> change_same_chars(T, change_first_char(H, Str2)).

is_anagram(Str, Str) -> false;
is_anagram(Str1, Str2) -> 
    change_same_chars(Str1, Str2) =:= change_same_chars(Str2, Str1).

find_anagrams(Subject, Candidates) -> 
    lists:filter(fun(Word) -> is_anagram(string:lowercase(Subject), string:lowercase(Word)) end,  Candidates).
