-module(zebra).

-compile([export_all]).

%-type color() :: red | green | ivory | yellow | blue.
%-type nation() :: english | spaniard | ukrainian | norwegian | japanese.
%-type pet() :: dog | snail | fox | horse | zebra.
%-type house_num() :: 1..5.
%-type beverage() :: tea | coffee | milk | orange_juice | water.
%-type cigarette() :: old_gold | kools | chesterfields | lucky_strike | parliaments.

-define(STATEMENT_CONFIGS,
 [{{color, red}, {nation, english}},
  {{pet, dog},{nation, spaniard}},
  {{beverage, coffee}, {color, green}},
  {{nation, ukrainian}, {beverage, tea}},
  {{cigarette, old_gold}, {pet, snail}},
  {{cigarette, kools}, {color, yellow}},
  {{beverage, milk}, {house_num, 3}},
  {{nation, norwegian}, {house_num, 1}},
  {{beverage, orange_juice}, {cigarette, lucky_strike}},
  {{nation, japanese}, {cigarette, parliaments}},
  {{color, blue}, {house_num, 2}},
  {{color, green}, {color, ivory}, fun id/1},
  {{cigarette, chesterfields}, {pet, fox}, fun abs/1},
  {{cigarette, kools}, {pet, horse}, fun abs/1},
  {unique}
]).

-define(LIKELYHOOD, 5).

-define(STATEMENTS,
  lists:map(
  fun
    (Config) -> fun (Individual) -> statement(Config, Individual) end
  end,
  ?STATEMENT_CONFIGS
  )).

-define(INIT_VALUES, #{color => [red, green, ivory, yellow, blue],
                       nation => [english, spaniard, ukrainian, norwegian, japanese],
                       pet => [dog, snail, fox, horse, zebra],
                       house_num => lists:seq(1, 5),
                       beverage => [tea, coffee, milk, orange_juice, water],
                       cigarette => [old_gold, kools, chesterfields, lucky_strike, parliaments]}).
-define(SOLUTION, #{color => [yellow, blue, red, ivory, green],
                    nation => [norwegian, ukrainian, english, spaniard, japanese],
                    pet => [fox, horse, snail, dog, zebra],
                    house_num => lists:seq(1, 5),
                    beverage => [water, tea, milk, orange_juice, coffee],
                    cigarette => [kools, chesterfields, old_gold, lucky_strike, parliaments]}).

start(StartPop, Fuel) ->
  evolution_algo(start_population(StartPop),
                fun is_solution/1,
                fun selection/1,
                fun recombination/1,
                fun mutation/1,
                fun combinePopulation/3, Fuel).
%%% ===============================================
%%% Internal function
%%% ===============================================

print_with_score(Pop) ->
  List = lists:map(
    fun (X) -> {count_true_statement(X), X} end,
    Pop),
  io:format("-----------------------------PARENTS~p~n", [List]).

evolution_algo(Population, _, _, _, _, _, 0) ->
  print_with_score(Population);
evolution_algo(Population, IsSolution, Selection, Recombination, Mutation, CombinePopulation, Fuel) ->
  case lists:any(IsSolution, Population) of
    true -> 
      io:format("REAL SOLUTION"),
      lists:filter(fun (X) -> IsSolution(X) end, Population);
    false ->
      {Parents, Others} = Selection(Population),
      %print_with_score(Parents),
      %io:format("-----------------------------PARENTS ~p~n", [Parents]),
      Children = Recombination(Parents),
      %io:format("RECOMBINDED ~p~n", [Children]),
      Children2 = Mutation(Children),
      %io:format("Mutation ~p~n", [Children2]),
      NewPopulation = CombinePopulation(Parents, Others, Children2),
      %io:format("NewPopulation ~p~n", [NewPopulation]),
      evolution_algo(NewPopulation,
                     IsSolution,
                     Selection,
                     Recombination,
                     Mutation,
                     CombinePopulation, Fuel - 1)
  end.

-define(MIN_SCORE, 6).

selection(Population) ->
  lists:foldl(
  fun
    (Ind, {Parent, Others}) ->
      P = (count_true_statement(Ind) / length(?STATEMENT_CONFIGS)) * 100,
      Rand = rand:uniform(100) + rand:uniform(10),
      case (Rand < P) of
        true ->
          {[Ind | Parent], Others};
        false ->
          {Parent, [Ind | Others]}
      end
  end
).

choose_random(N, List) when N >= length(List) ->
  List;
choose_random(N, List) ->
  choose_random(N, List, []).

choose_random(0, _, Accu) -> Accu;
choose_random(N, List, Accu) ->
  Ind = rand:uniform(length(List)),
  Elem = lists:nth(Ind, List),
  choose_random(N - 1,
                lists:delete(Elem, List),
                [Elem | Accu]).

recombination([_]) -> error;
recombination(Individuals) ->
  Pairs = create_pairs(Individuals),
  lists:flatten(lists:map(fun parcial_combination/1, Pairs)).

start_population(N) ->
 [random_individual() || _ <- lists:seq(1, N)].

combinePopulation(Parents, Low, Children) ->
  N = length(Children),
  choose_random(2 * (N div 3), Parents)
    ++ Children
    ++ choose_random(N div 3, Low).

mutation(Population) ->
  N = length(Population),
  lists:map(
    fun (Ind) -> mutation_ind(Ind, rand:uniform(N)) end,
    Population).

mutation_ind(Individual, Rand) when (Rand rem ?LIKELYHOOD) =:= 0 ->
  RandAttr = lists:nth(rand:uniform(6), maps:keys(?INIT_VALUES)),
  [H | V] = maps:get(RandAttr, Individual),
  Individual#{RandAttr => V ++ [H]};
mutation_ind(Individual, _) -> Individual.


random_individual() ->
  maps:map(
    fun (_, V) -> randomize(V) end,
    ?INIT_VALUES
).

count_true_statement(Ind) ->
  lists:foldl(
    fun (Statement, Count) ->
      case Statement(Ind) of
        true -> Count + 1;
        false -> Count
      end
    end,
    0, ?STATEMENTS).

is_solution(Ind) ->
  count_true_statement(Ind) =:= length(?STATEMENT_CONFIGS).

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
  RandInd = rand:uniform(length(List) - 1) + 1,
  create_pairs([lists:nth(RandInd, List) | List]).

parcial_combination([Ind1, Ind2]) ->
  lists:foldl(
  fun
    (K, [Fst, Snd]) ->
      {L1, L2} = parcial_combination(maps:get(K, Ind1), maps:get(K, Ind2)),
      [Fst#{K => L1}, Snd#{K => L2}]
  end,
  [#{}, #{}],
  maps:keys(Ind1)).

parcial_combination([A1, A2, A3, A4, A5],
                    [B1, B2, B3, B4, B5]) ->
  {[NA1, NA4, NA5],
   [NB1, NB4, NB5]} = parcial_combination([A1, A4, A5], [A1, A4, A5], [], [B1, B4, B5], [B1, B4, B5], []),
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

substract(List, Sub) ->
  lists:foldl(
    fun(X, Acc) -> lists:delete(X, Acc) end,
    List,
    Sub).









  



%%% ===============================================
%%% Statement
%%% ===============================================

statement({unique}, Ind) ->
  maps:fold(
    fun (_, _, false) -> false;
        (_Attr, Values, true) ->
        lists:uniq(Values) =:= Values
    end,
    true,
    Ind);
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
