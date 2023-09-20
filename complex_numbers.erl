-module(complex_numbers).

-export([abs/1, add/2, conjugate/1, divide/2, equal/2, exp/1, imaginary/1, mul/2, new/2,
	 real/1, sub/2]).

square(N) -> N*N.

abs_sqaure({R, I}) -> square(R) + square(I).
abs({R, I}) -> math:sqrt( abs_sqaure({R, I}) ).

add({R1, I1}, {R2, I2}) -> {R1 + R2, I1 + I2}.

conjugate({R, I}) -> {R, -I}.

reciprocal(Z = {R, I}) -> {R / abs_sqaure(Z), -I / abs_sqaure(Z)}.

divide(Z1, Z2) -> mul(Z1, reciprocal(Z2)).

equal({R1, I1}, {R2, I2}, Delta) when (R1 - R2) < Delta, (I1 - I2) < Delta -> true;
equal(_,_,_) -> false.
equal(Z1, Z2) -> equal(Z1, Z2, 0.0001).

exp({R, I}) -> {math:exp(R) * math:cos(I), math:exp(R) * math:sin(I)}.

imaginary({_, I}) -> I.

mul({R1, I1}, {R2, I2}) -> {R1 * R2 - I1*I2, R1*I2 + R2*I1}.

new(R, I) -> {R, I}.

real({R, _}) -> R.

sub({R1, I1}, {R2, I2}) -> {R1 - R2, I1 - I2}.
