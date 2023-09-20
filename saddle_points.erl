-module(saddle_points).

-export([saddle_points/1]).

list_element(0, [H | _]) -> H;
list_element(N, List = [_ | T]) ->
    case length(List) of
        Length when N >= Length -> badarg;
        _ -> list_element(N - 1, T) end;
list_element(_ , _ ) -> badarg.

matrix_element({X,Y}, Matrix) -> list_element(Y, list_element(X, Matrix)).

max_in_list([H | []], N) when H > N -> H;
max_in_list([_ | []], N) -> N;
max_in_list([H | Tail], N) when H > N -> max_in_list(Tail, H);
max_in_list([_ | Tail], N) -> max_in_list(Tail, N);
max_in_list(_, _) -> badarg.

max_in_list(List) -> max_in_list(List, list_element(0, List)).

min_in_list([H | []], N) when H < N -> H;
min_in_list([_ | []], N) -> N;
min_in_list([H | Tail], N) when H < N -> min_in_list(Tail, H);
min_in_list([_ | Tail], N) -> min_in_list(Tail, N);
min_in_list(_, _) -> badarg.

min_in_list(List) -> min_in_list(List, list_element(0, List)).

get_index(_, [], _) -> [];
get_index(Num, [H | Tail], Count) -> 
    case Num =:= H of
        true -> [Count | get_index(Num, Tail, Count + 1)];
        false -> get_index(Num, Tail, Count + 1) end.

get_index(Num, List) -> get_index(Num, List, 0).

get_max_index(List) -> get_index( max_in_list(List), List ).


column_to_list([], _) -> [];
column_to_list([Row | Matrix], Column) ->  [list_element(Column, Row)] ++ column_to_list(Matrix, Column).

get_min_index_column(Matrix, Column) -> 
    ColumnList = column_to_list(Matrix, Column),
    %io:format("ColumnList ~w\n", [ColumnList]),
    MinElem = min_in_list(ColumnList),
    %io:format("MinElem ~w\n", [MinElem]),
    get_index(MinElem, ColumnList).


contains(_, []) -> false;
contains(N, [N | _]) -> true;
contains(N, [_ | T]) -> contains(N, T).

repeater([], _, _) -> [];
repeater([ColumnIndex | ColumnIndices], Matrix, RowIndex ) ->
    MinIndicesInColumn = get_min_index_column(Matrix, ColumnIndex),
    %io:format("RowIndex ~w\n", [RowIndex]),
    %io:format("ColumnIndex ~w\n", [ColumnIndex]),
    %io:format("MinIndicesInColumn ~w\n", [MinIndicesInColumn]),
    case contains(RowIndex, MinIndicesInColumn) of
        true -> [{RowIndex, ColumnIndex} | repeater(ColumnIndices, Matrix, RowIndex)];
        false -> repeater(ColumnIndices, Matrix, RowIndex) end.

saddle_points(_, [], _) -> []; 
saddle_points(Matrix, [Row | Rows], RowIndex) -> 
    ColumnIndices = get_max_index(Row),
    %io:format("columnIndex: ~w \n", [ColumnIndices]),
    %io:format("rowindex: ~w \n", [RowIndex]),
    TupleList = repeater(ColumnIndices, Matrix, RowIndex),
   % io:format("tuplelist : ~w\n", [TupleList]),

    TupleList ++ saddle_points(Matrix, Rows, RowIndex + 1).

saddle_points(Mat) -> saddle_points(Mat, Mat, 0).