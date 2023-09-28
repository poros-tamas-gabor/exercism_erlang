-module(grains).

-export([square/1, total/0]).

pow2_ad_n(0, Acc) -> Acc;
pow2_ad_n(N, Acc) when N > 0 -> pow2_ad_n(N - 1, 2*Acc);
pow2_ad_n(_, _) -> {error, badarg}.

pow2_ad_n(N) -> pow2_ad_n(N, 1).

square(Square) when Square > 0, Square < 65 -> pow2_ad_n(Square - 1);
square(_) -> {error, "square must be between 1 and 64"}.

total() -> pow2_ad_n(64) - 1.
