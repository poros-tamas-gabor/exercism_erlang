-module(zebra).

-export([start/0,
         start/1]).

-define(STATEMENT_CONFIGS,
 [
  {unique},
  {{color, red}, {nation, english}},
  {{pet, dog},{nation, spaniard}},
  {{beverage, coffee}, {color, green}},
  {{nation, ukrainian}, {beverage, tea}},
  {{color, green}, {color, ivory}, fun id/1},
  {{cigarette, old_gold}, {pet, snail}},
  {{cigarette, kools}, {color, yellow}},
  {{beverage, milk}, {house_num, 3}},
  {{nation, norwegian}, {house_num, 1}},
  {{cigarette, chesterfields}, {pet, fox}, fun abs/1},
  {{cigarette, kools}, {pet, horse}, fun abs/1},
  {{beverage, orange_juice}, {cigarette, lucky_strike}},
  {{nation, japanese}, {cigarette, parliaments}},
  {{color, blue}, {nation, norwegian}, fun id/1}
]).

-define(LIKELIHOOD, 5).
-define(DEFAULT_STARTING_POPULATION_SIZE, 20).

-define(STATEMENTS,
  lists:map(
  fun
    (Config) -> fun (Individual) -> statement(Config, Individual) end
  end,
  ?STATEMENT_CONFIGS
  )).

-define(INIT_VALUES, 
  #{color => [red, green, ivory, yellow, blue],
    nation => [english, spaniard, ukrainian, norwegian, japanese],
    pet => [dog, snail, fox, horse, zebra],
    house_num => lists:seq(1, 5),
    beverage => [tea, coffee, milk, orange_juice, water],
    cigarette => [old_gold, kools, chesterfields, lucky_strike, parliaments]}).

-define(SOLUTION, 
  #{color => [yellow, blue, red, ivory, green],
    nation => [norwegian, ukrainian, english, spaniard, japanese],
    pet => [fox, horse, snail, dog, zebra],
    house_num => lists:seq(1, 5),
    beverage => [water, tea, milk, orange_juice, coffee],
    cigarette => [kools, chesterfields, old_gold, lucky_strike, parliaments]}).

-type individual() :: #{color := list(),
                        nation := list(),
                        pet := list(),
                        house_num := list(),
                        beverage := list(),
                        cigarette := list(),
                        score := integer() % Number of true statements
                      }.

-define(ATTRIBUTES, [color,
                     nation,
                     pet,
                     house_num,
                     beverage,
                     cigarette]).

%%% ===============================================
%%% API
%%% ===============================================

start() ->
  start(?DEFAULT_STARTING_POPULATION_SIZE).

start(PopulationNumber) ->
  evolution_algo(PopulationNumber,
                 init_population(PopulationNumber),
                 fun is_solution/1,
                 fun selection/1,
                 fun recombination/1,
                 fun mutation/1,
                 fun combinePopulation/4).
%%% ===============================================
%%% Internal function
%%% ===============================================

init_population(N) ->
  [random_individual() || _ <- lists:seq(1, N)].

evolution_algo(PopulationNumber,
               Population,
               IsSolution,
               Selection,
               Recombination,
               Mutation,
               CombinePopulation) ->
  case lists:any(IsSolution, Population) of
    true -> 
      io:format("Zebra puzzle solution~n"),
      lists:filter(fun (X) -> IsSolution(X) end, Population);
    false ->
      {Parents, Others} = Selection(Population),
      print_stat(Parents),
      Children = Recombination(Parents),
      Children2 = Mutation(Children),
      NewPopulation = CombinePopulation(PopulationNumber,
                                        Parents,
                                        Others,
                                        Children2),
      evolution_algo(PopulationNumber,
                     NewPopulation,
                     IsSolution,
                     Selection,
                     Recombination,
                     Mutation,
                     CombinePopulation)
  end.

%%% SELECTION
selection(Population) ->
  {Parents, Others} = lists:foldl(
    fun
      (#{score := Score} = Ind, {Parents, Others}) ->
        ScoreRatio = (Score / length(?STATEMENT_CONFIGS)) * 100,
        BonusScore = rand:uniform(10),
        Rand = rand:uniform(100) - BonusScore,
        case (Rand < ScoreRatio) of
          true ->
            {[Ind | Parents], Others};
          false ->
            {Parents, [Ind | Others]}
        end
    end,
    {[], []},
    Population),
  case length(Parents) < 2 of
    true -> selection(Population);
    false -> {Parents, Others}
  end. 

%%% RECOMBINATION
recombination([_]) -> error;
recombination(Individuals) ->
  Pairs = create_pairs(Individuals),
  lists:flatten(lists:map(fun parcial_combination/1, Pairs)).

%%% COMBINE POPULATION
combinePopulation(PopulationNumber, Parents, Others, Children) ->
  ChildrenNum = length(Children),
  case ChildrenNum > PopulationNumber of
    true -> lists:uniq(Children);
    false -> lists:uniq(Children ++ Parents ++ Others)
  end.

%%% MUTATION
mutation(Population) when is_list(Population) ->
  lists:map(
    fun (Ind) -> mutation(Ind) end,
    Population);
mutation(Individual) when is_map(Individual) ->
  Rand = rand:uniform(100),
  case Rand =< ?LIKELIHOOD of
    true ->
      RandAttr = lists:nth(rand:uniform(6), ?ATTRIBUTES),
      [H | V] = maps:get(RandAttr, Individual),
      Mutated = Individual#{RandAttr => V ++ [H]},
      set_score(Mutated);
    _ ->
      Individual
  end.

random_individual() ->
  Ind = maps:map(
    fun (_, V) -> randomize(V) end,
    ?INIT_VALUES),
  set_score(Ind).

count_true_statement(Ind) ->
  lists:foldl(
    fun (Statement, Count) ->
      case Statement(Ind) of
        true -> Count + 1;
        false -> Count
      end
    end,
    0, ?STATEMENTS).

is_solution(#{score := Score} = _Ind) ->
  Score =:= length(?STATEMENT_CONFIGS).

create_pairs(List) when (length(List) rem 2) =:= 0 ->
  [[] | Pairs] = lists:foldl(
  fun
    (Ind, [[] | T]) ->
      [[Ind] | T];
    (Ind, [[Pair] | T]) ->
      [[], [Ind, Pair] | T]
  end,
  [[]],
  List),
  Pairs;
create_pairs(List) ->
  Last = lists:last(List),
  create_pairs([Last | List]).

parcial_combination([Ind1, Ind2]) ->
  Children = lists:foldl(
  fun
    (K, [Child1, Child2]) ->
      {L1, L2} = parcial_combination(maps:get(K, Ind1), maps:get(K, Ind2)),
      [Child1#{K => L1}, Child2#{K => L2}]
  end,
  [#{}, #{}],
  ?ATTRIBUTES),
  lists:map(fun set_score/1, Children).

parcial_combination([A1, A2, A3, A4, A5],
                    [B1, B2, B3, B4, B5]) ->
  {[NA1, NA4, NA5],
   [NB1, NB4, NB5]} = parcial_combination([A1, A4, A5],
                                          [A1, A4, A5], [],
                                          [B1, B4, B5],
                                          [B1, B4, B5], []),
  {[NA1, B2, B3, NA4, NA5],
   [NB1, A2, A3, NB4, NB5]}.

parcial_combination([], _As, Aaccu, [], _Bs, Baccu) -> {Aaccu, Baccu};
parcial_combination(TA, _As, Aaccu, [], _Bs, Baccu) -> {Aaccu ++ TA, Baccu};
parcial_combination([], _As, Aaccu, TB, _Bs, Baccu) -> {Aaccu, Baccu ++ TB};
parcial_combination([HA | TA], As, Aaccu, [HB | TB], Bs, Baccu) ->
  case {lists:member(HA, Bs), lists:member(HB, As)} of
    {true, true} ->
      parcial_combination(TA, As, Aaccu ++ [HA], TB, Bs, Baccu ++ [HB]);
    {false, false} ->
      parcial_combination(TA, As, Aaccu ++ [HB], TB, Bs, Baccu ++ [HA]);
    {true, false} ->
      parcial_combination(TA, As, Aaccu ++ [HA], [HB | TB], Bs, Baccu);
    {false, true} ->
      parcial_combination([HA | TA], As, Aaccu, TB, Bs, Baccu ++ [HB])
  end.

set_score(Ind) ->
  Ind#{score => count_true_statement(Ind)}.

%%% ===============================================
%%% Statement
%%% ===============================================

statement({unique}, Ind) ->
  lists:foldl(
    fun 
      (_, false) -> false;
      (Attr, true) ->
        Values = maps:get(Attr, Ind),
        lists:uniq(Values) =:= Values
    end,
    true,
    ?ATTRIBUTES);
statement({{K1, V1},{K2, V2}}, Ind) ->
  L1 = maps:get(K1, Ind),
  L2 = maps:get(K2, Ind),
  statement(V1, L1, V2, L2);
statement({{K1, V1}, {K2, V2}, Fun}, Ind) ->
  HouseNums = maps:get(house_num, Ind),
  L1 = maps:get(K1, Ind),
  L2 = maps:get(K2, Ind),
  Ind1 = index(V1, L1),
  Ind2 = index(V2, L2),
  HN1 = lists:nth(Ind1, HouseNums),
  HN2 = lists:nth(Ind2, HouseNums),
  (Fun(HN1 - HN2) == 1).
  
statement(V1, [V1 | _], V2, [V2 | _]) -> true;
statement(_, [], _, []) -> false;
statement(V1, [_ | T1], V2, [_ | T2]) -> statement(V1, T1, V2, T2).

%%% ===============================================
%%% Utils
%%% ===============================================

id(X) -> X.

index(E, List) ->
  index(E, List, 1).

index(E, [E | _], Num) ->
  Num;
index(E, [_ | T], Num) ->
  index(E, T, Num + 1);
index(_, [], _) ->
  error.

randomize(List) -> randomize([], List).
randomize(Randomized, []) -> Randomized;
randomize(Randomized, Ordered) ->
  RandInd = rand:uniform(length(Ordered)),
  Rand = lists:nth(RandInd, Ordered),
  randomize([Rand | Randomized], lists:delete(Rand, Ordered)).

substract(List, Sub) ->
  lists:foldl(
    fun(X, Acc) -> lists:delete(X, Acc) end,
    List,
    Sub).

print_stat(Population) ->
  Num = length(Population),
  Scores = lists:map(
    fun (#{score := Score}) -> Score end,
    Population),
  io:format("Parent values {min, ~p} {avg, ~p} {max, ~p}~n",
           [lists:min(Scores), lists:sum(Scores)/Num, lists:max(Scores)]).