:- use_module(library(lists)).
:- use_module(library(between)).

% up(+Board, +Position, -UpPosition).
up(_, Row-Col, UpRow-Col) :-
    Row > 0,
    UpRow is Row - 1.

% down(+Board, +Position, -DownPosition).
down(Board, Row-Col, DownRow-Col) :-
    length(Board, Rows),
    LastRow is Rows - 1,
    Row < LastRow,
    DownRow is Row + 1.

% left(+Board, +Position, -LeftPosition).
left(_, Row-Col, Row-LeftCol) :-
    Col > 0,
    LeftCol is Col - 1.

% right(+Board, +Position, -RightPosition).
right(Board, Row-Col, Row-RightCol) :-
    length(Board, Cols),
    LastCol is Cols - 1,
    Col < LastCol,
    RightCol is Col + 1.


% piece(?Board, +Position, ?Piece).
get_piece(Board, Row-Col, Piece) :-
    nth0(Row, Board, BoardRow),
    nth0(Col, BoardRow, Piece).

% same_color(+Board, +Position1, +Position2).
same_color(Board, Position1, Position2) :-
    get_piece(Board, Position1, Piece),
    get_piece(Board, Position2, Piece).


% neighbor(+Board, +Position, -NeighborPosition).
neighbor(Board, Position, NeighborPosition) :-
    up(Board, Position, NeighborPosition),
    same_color(Board, Position, NeighborPosition).

neighbor(Board, Position, NeighborPosition) :-
    down(Board, Position, NeighborPosition),
    same_color(Board, Position, NeighborPosition).

neighbor(Board, Position, NeighborPosition) :-
    left(Board, Position, NeighborPosition),
    same_color(Board, Position, NeighborPosition).

neighbor(Board, Position, NeighborPosition) :-
    right(Board, Position, NeighborPosition),
    same_color(Board, Position, NeighborPosition).

% flood_fill(+GameState, +ToFill, -Filled).
flood_fill(Board, ToFill, Filled) :-
    flood_fill_aux(Board, ToFill, [], Filled).

flood_fill_aux(_, [], Filled, Filled).
flood_fill_aux(Board, [Position | T], Acc, Filled) :-
    findall(NeighborPosition, (neighbor(Board, Position, NeighborPosition), \+member(NeighborPosition, T), \+member(NeighborPosition, Acc)), Neighbors),
    append(T, Neighbors, ToVisit),
    flood_fill_aux(Board, ToVisit, [Position | Acc], Filled).

% replace(+List, +Element, +Index, -NewList).
replace(List, Element, Index, NewList) :-
    nth0(Index, List, _, _L),
    nth0(Index, NewList, Element, _L).


% replace_piece(+Board, +Piece, +Position, -NewBoard).
replace_piece(Board, Piece, RowPos-ColPos, NewBoard) :-
    nth0(RowPos, Board, Row),
    replace(Row, Piece, ColPos, NewRow),
    replace(Board, NewRow, RowPos, NewBoard).


% move_piece(+Board, +Piece, +SourcePos, +DestPos, -NewBoard).
move_piece(Board, Piece, SourcePos, DestPos, NewBoard) :-
    replace_piece(Board, empty, SourcePos, _B),
    replace_piece(_B, Piece, DestPos, NewBoard).

% check_larger_group(+Board, +SourcePos, +DestPos).
check_larger_group(Board, SourcePos, DestPos) :-
    flood_fill(Board, [SourcePos], SourceFilled),
    length(SourceFilled, SourceSize),
    get_piece(Board, SourcePos, Piece),
    move_piece(Board, Piece, SourcePos, DestPos, NewBoard),
    flood_fill(NewBoard, [DestPos], DestFilled),
    length(DestFilled, DestSize),
    DestSize > SourceSize.

% valid_position(+Board, ?Position).
valid_position(Board, RowPos-ColPos) :-
    length(Board, BoardSize),
    N1 is BoardSize - 1,
    between(0, N1, RowPos),
    between(0, N1, ColPos).

% player_color(?Player, ?Piece).
player_color(1, red).
player_color(2, blue).


% check_player_piece(+Board, +Player, +Position).
check_player_piece(Board, Player, Position) :-
    get_piece(Board, Position, Piece),
    player_color(Player, Piece).

% empty(+Board, +Position).
is_empty(Board, Position) :-
    get_piece(Board, Position, empty).

% can_move(+GameState, +Move).
can_move(Board-Player, SourceRow-SourceCol-DestRow-DestCol) :-
    valid_position(Board, SourceRow-SourceCol),
    valid_position(Board, DestRow-DestCol),
    check_player_piece(Board, Player, SourceRow-SourceCol),
    is_empty(Board, DestRow-DestCol),
    check_larger_group(Board, SourceRow-SourceCol, DestRow-DestCol).

% valid_moves(+GameState, +Player, -ListOfMoves).
valid_moves(Board-_, Player, ListOfMoves) :-
    findall(SourceRow-SourceCol-DestRow-DestCol, can_move(Board-Player, SourceRow-SourceCol-DestRow-DestCol), ListOfMoves).


% change_player(+Player, -NewPlayer).
change_player(1, 2).
change_player(2, 1).


% move(+GameState, +Move, -NewGameState).
move(Board-Player, SourceRow-SourceCol-DestRow-DestCol, NewBoard-NewPlayer) :-
    can_move(Board-Player, SourceRow-SourceCol-DestRow-DestCol), % valid_moves(Board-Player, Player, ValidMoves), member(Move, ValidMoves),
    player_color(Player, Piece),
    move_piece(Board, Piece, SourceRow-SourceCol, DestRow-DestCol, NewBoard),
    change_player(Player, NewPlayer).


% get_piece_position(+Row, +Piece, -PieceCol)
get_piece_col(Row, Piece, PieceCol) :-
    nth0(PieceCol, Row, Piece).

invert(List, Inverted) :-
    invert_aux(List, [], Inverted).

invert_aux([], Inverted, Inverted).
invert_aux([H | T], Acc, Inverted) :-
    invert_aux(T, [H | Acc], Inverted).

flatten(NestedList, FlatList) :-
    flatten_aux(NestedList, [], FlatListInverted),
    invert(FlatListInverted, FlatList).

flatten_aux([], FlatList, FlatList).
flatten_aux([H | T], Acc, FlatList) :-
    invert(H, HInverted),
    append(HInverted, Acc, Acc1),
    flatten_aux(T, Acc1, FlatList).    

get_piece_position(Board, Piece, PieceRow-PieceCol) :-
    length(Board, BoardSize),
    flatten(Board, FlatBoard),
    nth0(Index, FlatBoard, Piece),
    PieceRow is Index div BoardSize,
    PieceCol is Index mod BoardSize.

% ha forma de fazer melhor ->findall de todas as posicoes e length da lista
player_pieces_number(Board, Piece, N) :-
    flatten(Board, FlatBoard),
    player_pieces_number_aux(FlatBoard, Piece, 0, N).

player_pieces_number_aux([], _, N, N).

player_pieces_number_aux([Piece | T], Piece, Acc, N) :-
    Acc1 is Acc + 1,
    player_pieces_number_aux(T, Piece, Acc1, N),
    !.

player_pieces_number_aux([_ | T], Piece, Acc, N) :-
    player_pieces_number_aux(T, Piece, Acc, N). 

% game_over(+GameState, -Winner)
game_over(Board-Player, Winner) :-
    change_player(Player, Winner),
    player_color(Winner, Piece),
    get_piece_position(Board, Piece, Position),
    !,
    flood_fill(Board, [Position], Group),
    player_pieces_number(Board, Piece, N),
    length(Group, N).

