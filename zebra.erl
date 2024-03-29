-module(zebra).

-compile([export_all]).

%-type color() :: red | green | ivory | yellow | blue.
%-type nation() :: english | spaniard | ukrainian | norwegian | japanese.
%-type pet() :: dog | snail | fox | horse | zebra.
%-type house_num() :: 1..5.
%-type beverage() :: tea | coffee | milk | orange_juice | water.
%-type cigarette() :: old_gold | kools | chesterfields | lucky_strike | parliaments.

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

-define(FALSE_STATEMENTS,
  [fun neg_statement_2nd/1,
   fun neg_statement_3rd/1,
   fun neg_statement_4th/1,
   fun neg_statement_5th/1,
   fun neg_statement_6th/1,
   fun neg_statement_7th/1,
   fun neg_statement_8th/1,
   fun neg_statement_9th/1,
   fun neg_statement_10th/1,
   fun neg_statement_11th/1,
   fun neg_statement_12th/1,
   fun neg_statement_13th/1,
   fun neg_statement_14th/1,
   fun neg_statement_15th/1]).

-define(INIT_VALUES, #{color => [red, green, ivory, yellow, blue],
                       nation => [english, spaniard, ukrainian, norwegian, japanese],
                       pet => [dog, snail, fox, horse, zebra],
                       house_num => lists:seq(1, 5),
                       beverage => [tea, coffee, milk, orange_juice, water],
                       cigarette => [old_gold, kools, chesterfields, lucky_strike]}).
-define(PROPERTIES, [color, nation, pet, house_num, beverage, cigarette]).
-define(INIT_HOUSE, #{color => undefined,
                      nation => undefined,
                      pet => undefined,
                      house_num => undefined,
                      beverage => undefined,
                      cigarette => undefined}).
-define(INIT_NODE, #{g => 0, h => 0,
                     houses => lists:duplicate(5, ?INIT_HOUSE),
                     values => ?INIT_VALUES}).

start() ->
  general_graph_search([start_node()],
                        fun evaluation_comp/2,
                        fun is_solution/1,
                        fun create_child/1, 0).

%%% ===============================================
%%% Internal function
%%% ===============================================

general_graph_search(Open, EvaluationComp, _, _, 2000) ->
  Current = min_f(EvaluationComp, Open),
  io:format("----------------------------------CURRENT: ~p EVAL ~n~p~nFuel:~p~n", [Current, evaluation_fun(Current), 2000]),
  {error, no_solution};
general_graph_search(Open, EvaluationComp, IsSolution, CreateChildren, Fuel) ->
  %io:format("----------------------------------OPEN: ~p~nFuel:~p~n", [Open, Fuel]),
  Current = min_f(EvaluationComp, Open),
  io:format("----------------------------------CURRENT: ~p~nFuel:~p~n", [(evaluation_fun(Current)), Fuel]),
  case IsSolution(Current) of
    true ->
      {ok, Current};
    false ->
      Open2 = lists:delete(Current, Open),
      Children = CreateChildren(Current),
      %io:format("Child: ~p~n~n", [Child]),
      general_graph_search(Children ++ Open2,
                           EvaluationComp,
                           IsSolution,
                           CreateChildren,
                           Fuel + 1)
  end.

min_f(Comp, List) ->
  lists:foldl(Comp, hd(List), List).

evaluation_comp(Node1, Node2) ->
  case evaluation_fun(Node1) < evaluation_fun(Node2) of
    true -> Node1;
    false -> Node2
  end.

create_children(#{houses := Houses, g := G, values := Values} = Node) ->
  {{Prop1, Value1}, Values1} = choose_random_value(Values),
  {{Prop2, Value2}, Values2} = choose_random_value(Values1),



  RandHouse = random(Houses),
  case {Prop1 =/= Prop2, RandHouse} of
    {true, #{Prop1 := undefined,
             Prop2 := undefined}} ->
      Child = Node#{g := G + 1,
                    houses := [RandHouse#{Prop1 := Value1,
                                         Prop2 := Value2} |
                               lists:delete(RandHouse, Houses)],
                    values := Values2},
      case (NewH = num_of_true_statement(Child, ?STATEMENTS)) > H of
        true -> Child#{h := NewH};
        false -> create_child(Node)
      end;

    _ -> create_child(Node)
  end.

choose_random_value(#{color := [],
                      nation := [],
                      pet := [],
                      house_num := [],
                      beverage := [],
                      cigarette := []}) ->
  {error, no_value};

choose_random_value(Values) ->
  RandProp = random(?PROPERTIES),
  #{RandProp := PropValues} = Values,
  case PropValues of
    [] -> choose_random_value(Values);
    _ ->
      RandValue = random(PropValues),
      {{RandProp, RandValue}, 
        Values#{RandProp := lists:delete(RandValue,
                                         PropValues)}}
  end.

random(List) ->
  lists:nth(rand:uniform(length(List)), List).

evaluation_fun(#{g := G} = Node) ->
  F = 10 * (14 - num_of_true_statement(Node, ?STATEMENTS)) + G + 1000 * num_of_true_statement(Node, ?FALSE_STATEMENTS),
  %io:format("evaluation_fun: ~p~p~n", [Node, F]),
  F.


num_of_true_statement(Node, Statements) ->
  lists:foldl(
    fun (Statement, Count) ->
      case Statement(Node) of
        true -> Count + 1;
        false -> Count
      end
    end,
    0, Statements).

is_solution(Node) ->
  lists:foldl(
    fun
      (Statement, true) -> Statement(Node);
      (_, false) -> false
    end,
    true,
    ?STATEMENTS).

start_node() ->
  ?INIT_NODE.
%%% ===============================================
%%% Statements
%%% ===============================================

statement(Node, {{Prop1, Val1}, {Prop2, Val2}}) ->
  lists:any(
    fun (#{Prop1 := Val1, Prop2 := Val2}) -> true;
        (_) -> false end,
    Houses).

statement_2nd(#{houses := Houses}) ->
  lists:any(
    fun (#{color := red, nation := english}) -> true;
        (_) -> false end,
    Houses).

neg_statement_2nd(#{houses := Houses}) ->
  lists:any(
    fun (#{color := red, nation := Nation}) when Nation =/= english, Nation =/= undefined -> true;
        (#{color := Color, nation := english}) when Color =/= red, Color =/= undefined -> true;
        (_) -> false
    end,
    Houses).

statement_3rd(#{houses := Houses}) ->
  lists:any(
    fun (#{pet := dog, nation := spaniard}) -> true;
        (_) -> false end,
    Houses).

neg_statement_3rd(#{houses := Houses}) ->
  lists:any(
    fun (#{pet := Pet, nation := spaniard}) when Pet =/= dog, Pet =/= undefined -> true;
        (#{pet := dog, nation := Nation}) when Nation =/= spaniard, Nation =/= undefined -> true;
        (_) -> false
    end,
    Houses).

statement_4th(#{houses := Houses}) ->
  lists:any(
  fun (#{beverage := coffee, color := green}) -> true;
      (_) -> false end,
  Houses).

neg_statement_4th(#{houses := Houses}) ->
  lists:any(
    fun (#{beverage := Beverage, color := green}) when Beverage =/= coffee, Beverage =/= undefined -> true;
        (#{beverage := coffee, color := Color}) when Color =/= green, Color =/= undefined -> true;
        (_) -> false
    end,
    Houses).

statement_5th(#{houses := Houses}) ->
  lists:any(
  fun (#{nation := ukrainian, beverage := tea}) -> true;
      (_) -> false end,
  Houses).

neg_statement_5th(#{houses := Houses}) ->
  lists:any(
    fun (#{nation := Nation, beverage := tea}) when Nation =/= ukrainian, Nation =/= undefined -> true;
        (#{nation := ukrainian, beverage := Beverage}) when Beverage =/= tea, Beverage =/= undefined -> true;
        (_) -> false
    end,
    Houses).

statement_6th(#{houses := Houses}) ->
  Green = lists:search(
    fun (#{color := green}) -> true; (_) -> false end,
    Houses),
  Ivory = lists:search(
    fun (#{color := ivory}) -> true; (_) -> false end,
    Houses),
  case {Green, Ivory} of
    {{value, #{house_num := GN}}, {value, #{house_num := IN}}}
      when GN =/= undefined, IN =/= undefined, ((GN - IN) == 1) ->
      true;
    _ ->
      false
  end.

neg_statement_6th(#{houses := Houses}) ->
  Green = lists:search(
    fun (#{color := green}) -> true; (_) -> false end,
    Houses),
  Ivory = lists:search(
    fun (#{color := ivory}) -> true; (_) -> false end,
    Houses),
  case {Green, Ivory} of
    {{value, #{house_num := GN}}, {value, #{house_num := IN}}}
      when GN =/= undefined, IN =/= undefined, ((GN - IN) =/= 1) ->
      true;
    _ ->
      false
  end.

statement_7th(#{houses := Houses}) ->
  lists:any(
  fun (#{cigarette := old_gold, pet := snail}) -> true;
      (_) -> false end,
  Houses).

neg_statement_7th(#{houses := Houses}) ->
  lists:any(
    fun (#{cigarette := Cigarette, pet := snail})
          when Cigarette =/= old_gold, Cigarette =/= undefined -> true;
        (#{cigarette := old_gold, pet := Pet})
          when Pet =/= snail, Pet =/= undefined -> true;
        (_) -> false
    end,
    Houses).

statement_8th(#{houses := Houses}) ->
  lists:any(
  fun (#{cigarette := kools, color := yellow}) -> true;
      (_) -> false end,
  Houses).

neg_statement_8th(#{houses := Houses}) ->
  lists:any(
    fun (#{cigarette := Cigarette, color := yellow}) when Cigarette =/= kools, Cigarette =/= undefined -> true;
        (#{cigarette := kools, color := Color}) when Color =/= yellow, Color =/= undefined -> true;
        (_) -> false
    end,
    Houses).

statement_9th(#{houses := Houses}) ->
  lists:any(
  fun (#{beverage := milk, house_num := 3}) -> true;
      (_) -> false end,
  Houses).

neg_statement_9th(#{houses := Houses}) ->
  lists:any(
    fun (#{beverage := Beverage, house_num := 3}) when Beverage =/= milk, Beverage =/= undefined -> true;
        (#{beverage := milk, house_num := HouseNum}) when HouseNum =/= 3, HouseNum =/= undefined -> true;
        (_) -> false
    end,
    Houses).

statement_10th(#{houses := Houses}) ->
  lists:any(
  fun (#{nation := norwegian, house_num := 1}) -> true;
      (_) -> false end,
  Houses). 

neg_statement_10th(#{houses := Houses}) ->
  lists:any(
    fun (#{nation := Nation, house_num := 1}) when Nation =/= norwegian, Nation =/= undefined -> true;
        (#{nation := norwegian, house_num := HouseNum}) when HouseNum =/= 1, HouseNum =/= undefined -> true;
        (_) -> false
    end,
    Houses).

statement_11th(#{houses := Houses}) ->
  Chester = lists:search(
    fun (#{cigarette := chesterfields}) -> true; (_) -> false end,
    Houses),
  Fox = lists:search(
    fun (#{pet := fox}) -> true; (_) -> false end,
    Houses),
  case {Chester, Fox} of
    {{value, #{house_num := N1}}, {value, #{house_num := N2}}}
      when N1 =/= undefined, N2 =/= undefined, (abs(N1 - N2) == 1) ->
      true;
    _ ->
      false
  end.

neg_statement_11th(#{houses := Houses}) ->
  Chester = lists:search(
    fun (#{cigarette := chesterfields}) -> true; (_) -> false end,
    Houses),
  Fox = lists:search(
    fun (#{pet := fox}) -> true; (_) -> false end,
    Houses),
  case {Chester, Fox} of
    {{value, #{house_num := N1}}, {value, #{house_num := N2}}}
      when N1 =/= undefined, N2 =/= undefined, (abs(N1 - N2) =/= 1) ->
      true;
    _ ->
      false
  end.

statement_12th(#{houses := Houses}) ->
  Kools = lists:search(
    fun (#{cigarette := kools}) -> true; (_) -> false end,
    Houses),
  Horse = lists:search(
    fun (#{pet := horse}) -> true; (_) -> false end,
    Houses),
  case {Kools, Horse} of
    {{value, #{house_num := N1}}, {value, #{house_num := N2}}}
      when N1 =/= undefined, N2 =/= undefined, (abs(N1 - N2) == 1) ->
      true;
    _ ->
      false
  end.

neg_statement_12th(#{houses := Houses}) ->
  Kools = lists:search(
    fun (#{cigarette := kools}) -> true; (_) -> false end,
    Houses),
  Horse = lists:search(
    fun (#{pet := horse}) -> true; (_) -> false end,
    Houses),
  case {Kools, Horse} of
    {{value, #{house_num := N1}}, {value, #{house_num := N2}}}
      when N1 =/= undefined, N2 =/= undefined, (abs(N1 - N2) =/= 1) ->
      true;
    _ ->
      false
  end.

statement_13th(#{houses := Houses}) ->
  lists:any(
  fun (#{beverage := orange_juice, cigarette := lucky_strike}) -> true;
      (_) -> false end,
  Houses).

neg_statement_13th(#{houses := Houses}) ->
  lists:any(
    fun (#{beverage := Beverage, cigarette := lucky_strike}) when Beverage =/= orange_juice, Beverage =/= undefined -> true;
        (#{beverage := orange_juice, cigarette := Cigarette}) when Cigarette =/= lucky_strike, Cigarette =/= undefined -> true;
        (_) -> false
    end,
    Houses).

statement_14th(#{houses := Houses}) ->
  lists:any(
  fun (#{nation := japanese, cigarette := parliaments}) -> true;
      (_) -> false end,
  Houses).
neg_statement_14th(#{houses := Houses}) ->
  lists:any(
    fun (#{nation := Nation, cigarette := parliaments}) when Nation =/= japanese, Nation =/= undefined -> true;
        (#{nation := japanese, cigarette := Cigarette}) when Cigarette =/= parliaments, Cigarette =/= undefined -> true;
        (_) -> false
    end,
    Houses).

statement_15th(#{houses := Houses}) ->
  lists:any(
  fun (#{color := blue, house_num := 2}) -> true;
      (_) -> false end,
  Houses).
neg_statement_15th(#{houses := Houses}) ->
  lists:any(
    fun (#{color := Color, house_num := 2}) when Color =/= blue, Color =/= undefined -> true;
        (#{color := blue, house_num := HouseNum}) when HouseNum =/= 2, HouseNum =/= undefined -> true;
        (_) -> false
    end,
    Houses).