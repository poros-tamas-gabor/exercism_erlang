-module(minesweeper).

-export([annotate/1]).

-compile([export_all]).


convert_space_to_zero([]) -> [];
convert_space_to_zero([H | T]) when is_integer(H), H =:= $* -> [H | convert_space_to_zero(T)];
convert_space_to_zero([H | T]) when is_integer(H), H =:= $  -> [0 | convert_space_to_zero(T)];
convert_space_to_zero(_Minefield = [Row | Rows]) -> [convert_space_to_zero(Row) | convert_space_to_zero(Rows)].

collectMines([], _, _) -> [];
collectMines(_Row = [H | T], RowNum, ColNum) when is_integer(H), H =:= $* -> [{RowNum, ColNum} | collectMines(T, RowNum, ColNum + 1)];
collectMines(_Row = [H | T], RowNum, ColNum) when is_integer(H), H =:= $  -> collectMines(T, RowNum, ColNum + 1);
collectMines(_Minefield = [Row | Rows], RowNum, _) -> [collectMines(Row, RowNum, 0) | collectMines(Rows, RowNum + 1, 0)].

collectMines(Minefield) -> lists:flatten(collectMines(Minefield, 0, 0)).

annotate_adjacent_fields([], _, _, _) -> [];
annotate_adjacent_fields(_Row = [H | T], {MineRow, MineCol}, Row, Col) when is_integer(H) ->
    case ((abs(MineRow - Row) =< 1) and (abs(MineCol - Col) =< 1) and (H =/= $*)) of
        true -> [H + 1 | annotate_adjacent_fields(T, {MineRow, MineCol}, Row, Col + 1)];
        false -> [H | annotate_adjacent_fields(T, {MineRow, MineCol}, Row, Col + 1)]
    end;
annotate_adjacent_fields(_Minefield = [Row | Rows], {MineRow, MineCol}, RowNum, _) when is_list(Row) ->
    [annotate_adjacent_fields(Row, {MineRow, MineCol}, RowNum, 0) | annotate_adjacent_fields(Rows,  {MineRow, MineCol}, RowNum + 1, 0)].

annotate_adjacent_fields(Minefield, Coord) -> annotate_adjacent_fields(Minefield, Coord, 0, 0).

annotate_helper(ConvertedMinefield, []) -> ConvertedMinefield;
annotate_helper(ConvertedMinefield, [Mine | Mines]) -> annotate_helper(annotate_adjacent_fields(ConvertedMinefield, Mine), Mines).

convert_integer_to_str([]) -> [];
convert_integer_to_str(_Row = [H | T]) when is_integer(H) and (H == $*) -> "*" ++ convert_integer_to_str(T);
convert_integer_to_str(_Row = [H | T]) when is_integer(H) and (H == 0) -> " " ++ convert_integer_to_str(T);
convert_integer_to_str(_Row = [H | T]) when is_integer(H) -> integer_to_list(H)  ++ convert_integer_to_str(T);
convert_integer_to_str(_Minefield = [Row | Rows]) when is_list(Row) -> [ convert_integer_to_str(Row) | convert_integer_to_str(Rows)].

annotate(Minefield) -> 
    CollectedMines = collectMines(Minefield),
    ConvertedMinefield = convert_space_to_zero(Minefield),
    Annotated = annotate_helper(ConvertedMinefield, CollectedMines),
    convert_integer_to_str(Annotated).
