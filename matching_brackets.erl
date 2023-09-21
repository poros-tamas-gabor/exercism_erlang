-module(matching_brackets).

-export([is_paired/1]).

-compile([export_all]).

reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].

last_digit(Str) -> hd(reverse(Str)).

remove_first_last_digit(Str) -> reverse(tl(reverse(tl(Str)))).

find_pair([], _Char, _PairChar, _CharCounter, Index) -> Index; 
find_pair([H | _T], _Char, PairChar, 1, Index) when H =:= PairChar  -> Index; 
find_pair([H | T], Char, PairChar, CharCounter, Index) when H =:= Char  -> find_pair(T, Char, PairChar, CharCounter + 1, Index + 1); 
find_pair([H | T], Char, PairChar, CharCounter, Index) when H =:= PairChar  -> find_pair(T, Char, PairChar, CharCounter - 1, Index + 1); 
find_pair([_ | T], Char, PairChar, CharCounter, Index)   -> find_pair(T, Char, PairChar, CharCounter, Index + 1).

find_pair(Str, Char, PairChar) -> find_pair(Str, Char, PairChar, 0, 1).


sub_str([], _) -> [];
sub_str(Str, Num) ->
    case length(Str) < Num of
        true -> [];
        false -> string:substr(Str,Num) end.

sub_str([], _, _) -> [];
sub_str(Str, Num1, Num2) -> string:substr(Str,Num1, Num2).


is_bracket( Char ) -> is_first_bracket(Char) or is_last_bracket(Char).

is_first_bracket( $( ) -> true;
is_first_bracket( $[ ) -> true;
is_first_bracket( ${ ) -> true;
is_first_bracket( _ ) -> false.

is_last_bracket( $) ) -> true;
is_last_bracket( $] ) -> true;
is_last_bracket( $} ) -> true;
is_last_bracket( _ ) -> false.

bracket_pair( $( ) -> $);
bracket_pair( $[ ) -> $];
bracket_pair( ${ ) -> $}.

is_paired([]) -> true;
is_paired(Str = [H | T]) ->
    case is_bracket(H) of
        true -> 
            case is_first_bracket(H) of 
                true -> 
                    Index = find_pair(Str, H, bracket_pair(H)),
                    Fst = sub_str(Str, 1, Index),
                    Snd = sub_str(Str, Index + 1),
                    case last_digit(Fst) =:= bracket_pair(H)  of
                        true -> is_paired(remove_first_last_digit(Fst)) andalso is_paired(Snd);
                        false -> false end;

                false -> false end;
        false -> is_paired(T) end.



