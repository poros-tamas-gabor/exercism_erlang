-module(rational_numbers).

-export([absolute/1, add/2, divide/2, exp/2, mul/2, reduce/1, sub/2]).


absolute({N, D}) -> reduce({abs(N), abs(D)}).

add({N1, D1}, {N2, D2}) -> reduce({ N1 * D2 + N2 * D1, D1 * D2}).

reciprocal({N, D}) -> reduce({D, N}).


divide(R1, R2) -> reduce(mul(R1, reciprocal(R2))).

exp(_, 0) -> {1,1};
exp(Base, 1) -> reduce(Base);
exp(Base, Exponent) when is_integer(Exponent), Exponent > 1 -> reduce(mul( Base, exp(Base, Exponent - 1))); 
exp(Base, Exponent) when is_integer(Exponent), Exponent < 0 -> exp( reciprocal(Base) , abs(Exponent));

exp(Base, {N, D}) -> math:pow(math:pow(Base, N), 1 / D).




mul({N1, D1}, {N2, D2}) -> reduce({N1 * N2, D1 * D2}).

lnko(N, N) -> N;
lnko(N1, N2) when N1 > N2 -> lnko(N1 - N2, N2);
lnko(N1, N2) -> lnko(N1, N2 - N1).


reduce({0, _}) -> {0, 1};
reduce({N,D}) when N < 0, D < 0 -> reduce({abs(N), abs(D)});
reduce({N,D}) when N > 0, D < 0 -> reduce({-N, abs(D)});
reduce({N, D}) ->
    LNKO = lnko(abs(N), abs(D)),
    {N div LNKO, D div LNKO}.


sub({N1, D1}, {N2, D2}) -> reduce({N1 * D2- N2 * D1, D1 * D2}).
