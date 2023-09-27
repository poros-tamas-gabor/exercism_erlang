-module(calc).
-export([rpn/1, rpn_test/0]).


rpn(Expression) -> 
    [Top] = lists:foldl(fun(X, Acc) -> rpn(X, Acc) end, [], string:tokens(Expression, " ")),
    Top.

list_to_numeric(Num) -> 
    case string:to_float(Num) of
        {error, no_float} -> list_to_integer(Num);
        {Float, _} -> Float end.

rpn("+", [H2, H1 | Acc]) -> [H1 + H2 | Acc];
rpn("-", [H2, H1 | Acc]) -> [H1 - H2 | Acc];
rpn("/", [H2, H1 | Acc]) -> [H1 / H2 | Acc];
rpn("*", [H2, H1 | Acc]) -> [H1 * H2 | Acc];
rpn("^", [H2, H1 | Acc]) -> [math:pow(H1, H2) | Acc];
rpn("ln", [H1 | Acc]) -> [math:log(H1) | Acc];
rpn("log10", [H1 | Acc]) -> [math:log10(H1) | Acc];
rpn("sum", Acc) -> [lists:sum(Acc)];
rpn("prod", Acc) -> [lists:foldl(fun(X, Accum) -> X * Accum end, 1, Acc)];
rpn(Num, Acc) -> [list_to_numeric(Num) | Acc].

rpn_test() ->
5 = rpn("2 3 +"),
87 = rpn("90 3 -"),
-4 = rpn("10 4 3 + 2 * -"),
-2.0 = rpn("10 4 3 + 2 * - 2 /"),
ok = try
rpn("90 34 12 33 55 66 + * - +")
catch
error:{badmatch,[_|_]} -> ok
end,
4037 = rpn("90 34 12 33 55 66 + * - + -"),
8.0 = rpn("2 3 ^"),
true = math:sqrt(2) == rpn("2 0.5 ^"),
true = math:log(2.7) == rpn("2.7 ln"),
true = math:log10(2.7) == rpn("2.7 log10"),
50 = rpn("10 10 10 20 sum"),
10.0 = rpn("10 10 10 20 sum 5 /"),
1000.0 = rpn("10 10 20 0.5 prod"),
ok.