-module(zebra).

-compile([export_all]).

-define(STATEMENTS,
  [fun statement_2nd/1,
   fun statement_3rd/1,
   fun statement_4th/1,
   fun statement_5th/1,
   fun statement_6th/1,
   fun statement_7th/1,
   fun statement_8th/1,
   fun statement_9th/1,
   fun statement_10th/1,
   fun statement_11th/1,
   fun statement_12th/1,
   fun statement_13th/1,
   fun statement_14th/1,
   fun statement_15th/1]).


-record(values, {color = [red, green, ivory, yellow, blue],
                 nation = [english, spaniard, ukrainian, norwegian, japanese],
                 pet = [dog, snail, fox, horse, zebra],
                 house_num = lists:seq(1, 5),
                 beverage = [tea, coffee, milk, orange_juice, water],
                 cigarette = [old_gold, kools, chesterfields, lucky_strike]

        }).
%-type color() :: red | green | ivory | yellow | blue.
%-type nation() :: english | spaniard | ukrainian | norwegian | japanese.
%-type pet() :: dog | snail | fox | horse | zebra.
%-type house_num() :: 1..5.
%-type beverage() :: tea | coffee | milk | orange_juice | water.
%-type cigarette() :: old_gold | kools | chesterfields | lucky_strike | parliaments.

-record(house, {color = undefined,
                nation = undefined,
                pet = undefined,
                house_num = undefined,
                beverage = undefined,
                cigarette = undefined}).
-record(node, {g = 0, houses = lists:duplicate(5, #house{})}).

start() ->
  general_graph_search([start_node()],
                        fun evaluation_comp/2,
                        fun is_solution/1,
                        fun create_children/1).

%%% ===============================================
%%% Internal function
%%% ===============================================

general_graph_search([], _, _, _) -> {error, no_solution};
general_graph_search(Open, EvaluationComp, IsSolution, CreateChildren) ->
  Current = min_f(EvaluationComp, Open),
  case IsSolution(Current) of
    true ->
      {ok, Current};
    false ->
      Open2 = lists:delete(Current, Open),
      Children = CreateChildren(Current),
      general_graph_search(Open2 ++ Children,
                           EvaluationComp,
                           IsSolution,
                           CreateChildren)
  end.

min_f(Comp, List) ->
  lists:foldl(Comp, hd(List), List).

evaluation_comp(Node1, Node2) ->
  case evaluation_fun(Node1) < evaluation_fun(Node2) of
    true -> Node1;
    false -> Node2
  end.

create_children(Node) -> 

evaluation_fun(#node{g = G} = Node) ->
  det_num_of_false_statement(Node) + G.

det_num_of_false_statement(Node) ->
  lists:foldl(
    fun (Statement, Count) ->
      case Statement(Node) of
        true -> Count;
        false -> Count + 1
      end
    end,
    0, ?STATEMENTS).

is_solution(Node) ->
  lists:foldl(
    fun
      (Statement, true) -> Statement(Node);
      (_, false) -> false
    end,
    true,
    ?STATEMENTS).

start_node() ->
  #node{}.
%%% ===============================================
%%% Statements
%%% ===============================================

statement_2nd(#node{houses = Houses}) ->
  lists:any(
    fun (#house{color = red, nation = english}) -> true;
        (_) -> false end,
    Houses).

statement_3rd(#node{houses = Houses}) ->
  lists:any(
    fun (#house{pet = dog, nation = spaniard}) -> true;
        (_) -> false end,
    Houses).

statement_4th(#node{houses = Houses}) ->
  lists:any(
  fun (#house{beverage = coffee, color = green}) -> true;
      (_) -> false end,
  Houses).

statement_5th(#node{houses = Houses}) ->
  lists:any(
  fun (#house{nation = ukrainian, beverage = tea}) -> true;
      (_) -> false end,
  Houses).

statement_6th(#node{houses = Houses}) ->
  Green = lists:search(
    fun (#house{color = green}) -> true; (_) -> false end,
    Houses),
  Ivory = lists:search(
    fun (#house{color = ivory}) -> true; (_) -> false end,
    Houses),
  case {Green, Ivory} of
    {{value, #house{house_num = GN}}, {value, #house{house_num = IN}}}
      when GN =/= undefined, IN =/= undefined, ((GN - IN) == 1) ->
      true;
    _ ->
      false
  end.

statement_7th(#node{houses = Houses}) ->
  lists:any(
  fun (#house{cigarette = old_gold, pet = snail}) -> true;
      (_) -> false end,
  Houses).

statement_8th(#node{houses = Houses}) ->
  lists:any(
  fun (#house{cigarette = kools, color = yellow}) -> true;
      (_) -> false end,
  Houses).

statement_9th(#node{houses = Houses}) ->
  lists:any(
  fun (#house{beverage = milk, house_num = 3}) -> true;
      (_) -> false end,
  Houses).

statement_10th(#node{houses = Houses}) ->
  lists:any(
  fun (#house{nation = norwegian, house_num = 1}) -> true;
      (_) -> false end,
  Houses). 

statement_11th(#node{houses = Houses}) ->
  Chester = lists:search(
    fun (#house{cigarette = chesterfields}) -> true; (_) -> false end,
    Houses),
  Fox = lists:search(
    fun (#house{pet = fox}) -> true; (_) -> false end,
    Houses),
  case {Chester, Fox} of
    {{value, #house{house_num = N1}}, {value, #house{house_num = N2}}}
      when N1 =/= undefined, N2 =/= undefined, (abs(N1 - N2) == 1) ->
      true;
    _ ->
      false
  end.

statement_12th(#node{houses = Houses}) ->
  Kools = lists:search(
    fun (#house{cigarette = kools}) -> true; (_) -> false end,
    Houses),
  Horse = lists:search(
    fun (#house{pet = horse}) -> true; (_) -> false end,
    Houses),
  case {Kools, Horse} of
    {{value, #house{house_num = N1}}, {value, #house{house_num = N2}}}
      when N1 =/= undefined, N2 =/= undefined, (abs(N1 - N2) == 1) ->
      true;
    _ ->
      false
  end.

statement_13th(#node{houses = Houses}) ->
  lists:any(
  fun (#house{beverage = orange_juice, cigarette = lucky_strike}) -> true;
      (_) -> false end,
  Houses).

statement_14th(#node{houses = Houses}) ->
  lists:any(
  fun (#house{nation = japanese, cigarette = parliaments}) -> true;
      (_) -> false end,
  Houses).

statement_15th(#node{houses = Houses}) ->
  lists:any(
  fun (#house{color = blue, house_num = 2}) -> true;
      (_) -> false end,
  Houses).