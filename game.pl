:- use_module(library(lists)).
:- use_module(library(between)).

% GameState = Board-Player
% GameState = [[...], ...]-1/2

%     Col ------>
%  Row
%   |
%   |
%   |
%  \ /
%   .

% Position = Row-Col

% 0-0 0-1 0-2 0-3
% 1-0 1-1 1-2 1-3
% 2-0 2-1 2-2 2-3
% 3-0 3-1 3-2 3-3

% Move = SourcePosition-DestPosition
% Move = SourceRow-SourceCol-DestRow-DestCol



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
piece(Board, Row-Col, Piece) :-
    nth0(Row, Board, BoardRow),
    nth0(Col, BoardRow, Piece).

% same_color(+Board, +Position1, +Position2).
same_color(Board, Position1, Position2) :-
    piece(Board, Position1, Piece),
    piece(Board, Position2, Piece).


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


% flood_fill(+GameState, +Positions, -Filled).
flood_fill(Board, [Position | T], Filled) :-
    neighbor(Board, Position, NeighborPosition),
    \+member(NeighborPosition, [Position | T]),
    flood_fill(Board, [NeighborPosition, Position | T], Filled),
    !.

flood_fill(_, Filled, Filled).


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

% larger_group(+Board, +SourcePos, +DestPos).
larger_group(Board, SourcePos, DestPos) :-
    flood_fill(Board, [SourcePos], SourceFilled),
    length(SourceFilled, SourceSize),
    piece(Board, SourcePos, Piece),
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
player_color(1, blue).
player_color(2, red).

% player_piece(+Board, +Player, +Position).
player_piece(Board, Player, Position) :-
    piece(Board, Position, Piece),
    player_color(Player, Piece).

% empty(+Board, +Position).
empty(Board, Position) :-
    piece(Board, Position, empty).

% can_move(+GameState, +Move).
can_move(Board-Player, SourceRow-SourceCol-DestRow-DestCol) :-
    valid_position(Board, SourceRow-SourceCol),
    valid_position(Board, DestRow-DestCol),
    player_piece(Board, Player, SourceRow-SourceCol),
    empty(Board, DestRow-DestCol),
    larger_group(Board, SourceRow-SourceCol, DestRow-DestCol).

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
