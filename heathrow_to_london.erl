-module(heathrow_to_london).
-compile([export_all]).


read_text_file(FileName) ->
    try {ok, BinData} = file:read_file(FileName) of
        _ -> lists:map(fun(X) -> list_to_integer(X) end, string:tokens(binary_to_list(BinData), "\r\n"))
    catch
        _:_ -> [] end.


create_tuple_list() ->
    Costs = read_text_file("road.txt"),
    lists:reverse(lists:foldl(fun(X, Acc) -> create_tuple_list(X, Acc) end, [], lists:reverse(Costs))).

create_tuple_list(Xi, []) -> [{Xi}];
create_tuple_list(Xi, Acc = [{_, _, _} | _]) -> [{Xi} | Acc];
create_tuple_list(Bi, [{Xi} | T]) -> [{Xi, Bi} | T];
create_tuple_list(Ai, [{Xi, Bi} | T]) -> [{Xi, Bi, Ai} | T].


choose_path({Xi, Bi, Ai}, {{CostA, PathA}, {CostB, PathB}}) ->
    {NewCostA, NewPathA} = case Ai + CostA == min(Ai + CostA, Ai + Xi + CostB) of
        true -> {CostA + Ai, [{a, Ai} | PathA]};
        false -> {CostB + Ai + Xi, [{a, Ai}, {x, Xi} | PathB]} end,
    {NewCostB, NewPathB} = case Bi + CostB == min(Bi + CostB, Bi + Xi + CostA) of
        true -> {CostB + Bi, [{b, Bi} | PathB]};
        false -> {CostA + Bi + Xi, [ {b, Bi}, {x, Xi} | PathA]} end,
    {{NewCostA, NewPathA} , {NewCostB, NewPathB}};  

choose_path({_Xi, Bi, Ai}, {}) -> {{Ai, [{a ,Ai}]}, {Bi, [{b, Bi}]}}.

htl() -> 
    Paths = lists:foldl(fun(X, Acc) -> choose_path(X, Acc) end, {}, create_tuple_list()),
    case Paths =:= {} of
        true -> empty_graph;
        false -> {{CostA, PathA}, {CostB, PathB}} = Paths,
                min({CostA, PathA}, {CostB, PathB}) end.



