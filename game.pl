% game.pl


:- consult(utils).


% change_color(?Color1, ?Color2)
change_color(blue, red).
change_color(red, blue).


% change_player(?Player, ?NewPlayer)
change_player(1, 2).
change_player(2, 1).


% replace_piece(+Board, +Piece, +Position, -NewBoard)
replace_piece(Board, Piece, RowPos-ColPos, NewBoard) :-
    nth0(RowPos, Board, Row),
    replace(Row, Piece, ColPos, NewRow),
    replace(Board, NewRow, RowPos, NewBoard).


% move_piece(+Board, +Piece, +SourcePos, +DestPos, -NewBoard)
move_piece(Board, Piece, SourcePos, DestPos, NewBoard) :-
    replace_piece(Board, empty, SourcePos, _B),
    replace_piece(_B, Piece, DestPos, NewBoard).


% create_row(+RowSize, +CurrentColor, -Row)
create_row(RowSize, CurrentColor, Row) :-
    create_row_aux(RowSize, CurrentColor, [], Row).

create_row_aux(0, _, Row, Row).

create_row_aux(N, CurrentColor, Acc, Row) :-
    N > 0,
    N1 is N - 1,
    change_color(CurrentColor, NextColor),
    create_row_aux(N1, NextColor, [CurrentColor | Acc], Row).


% create_board(+BoardSize, -Board)
create_board(BoardSize, Board) :-
    create_board_aux(BoardSize, B),
    Last is BoardSize - 1,
    replace_piece(B, empty, 0-0, _B1),
    replace_piece(_B1, empty, 0-Last, _B2),
    replace_piece(_B2, empty, Last-0, _B3),
    replace_piece(_B3, empty, Last-Last, Board).

create_board_aux(BoardSize, Board) :-
    create_board_aux(BoardSize, 0, [], Board).

create_board_aux(N, Counter, Acc, List) :-
    Counter mod 2 =:= 0,
    create_row(N, red, Row),
    Counter < N - 1,
    create_board_aux(N, Counter + 1, [Row | Acc], List).

create_board_aux(N, Counter, Acc, [Row | Acc]) :-
    Counter mod 2 =:= 0,
    create_row(N, red, Row),
    Counter >= N - 1.

create_board_aux(N, Counter, Acc, List) :-
    Counter mod 2 =\= 0,
    create_row(N, blue, Row),
    Counter < N - 1,
    create_board_aux(N, Counter + 1, [Row | Acc], List).

create_board_aux(N, Counter, Acc, [Row | Acc]) :-
    Counter mod 2 =\= 0,
    create_row(N, blue, Row),
    Counter >= N - 1.


% valid_position(+Board, ?Position)
valid_position(Board, RowPos-ColPos) :-
    length(Board, BoardSize),
    N is BoardSize - 1,
    between(0, N, RowPos),
    between(0, N, ColPos).


% up(+Board, +Position, -UpPosition)
up(Board, Row-Col, UpRow-Col) :-
    UpRow is Row - 1,
    valid_position(Board, UpRow-Col).


% down(+Board, +Position, -DownPosition)
down(Board, Row-Col, DownRow-Col) :-
    DownRow is Row + 1,
    valid_position(Board, DownRow-Col).


% left(+Board, +Position, -LeftPosition)
left(Board, Row-Col, Row-LeftCol) :-
    LeftCol is Col - 1,
    valid_position(Board, Row-LeftCol).


% right(+Board, +Position, -RightPosition)
right(Board, Row-Col, Row-RightCol) :-
    RightCol is Col + 1,
    valid_position(Board, Row-RightCol).


% piece(?Board, ?Position, ?Piece) TODO
piece(Board, Row-Col, Piece) :-
    nth0(Row, Board, BoardRow),
    nth0(Col, BoardRow, Piece).


% empty(?Board, ?Position)
is_empty(Board, Position) :-
    piece(Board, Position, empty).


% player_piece(?Player, ?Piece)
player_piece(1, red).
player_piece(2, blue).


% check_player_piece(+Board, +Player, +Position)
check_player_piece(Board, Player, Position) :-
    piece(Board, Position, Piece),
    player_piece(Player, Piece).


% same_color(+Board, +Position1, +Position2)
same_color(Board, Position1, Position2) :-
    piece(Board, Position1, Piece),
    piece(Board, Position2, Piece).


% neighbor(+Board, +Position, -NeighborPosition)
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


% flood_fill(+GameState, +ToFill, -Filled)
flood_fill(Board, ToFill, Filled) :-
    flood_fill_aux(Board, ToFill, [], Filled).

flood_fill_aux(_, [], Filled, Filled).

flood_fill_aux(Board, [Position | T], Acc, Filled) :-
    findall(NeighborPosition, (neighbor(Board, Position, NeighborPosition), \+member(NeighborPosition, T), \+member(NeighborPosition, Acc)), Neighbors),
    append(T, Neighbors, ToVisit),
    flood_fill_aux(Board, ToVisit, [Position | Acc], Filled).


% check_larger_group(+Board, +SourcePos, +DestPos)
check_larger_group(Board, SourcePos, DestPos) :-
    flood_fill(Board, [SourcePos], SourceFilled),
    length(SourceFilled, SourceSize),
    piece(Board, SourcePos, Piece),
    move_piece(Board, Piece, SourcePos, DestPos, NewBoard),
    flood_fill(NewBoard, [DestPos], DestFilled),
    length(DestFilled, DestSize),
    DestSize > SourceSize.


% can_move(+GameState, +Move)
can_move(Board-Player, SourceRow-SourceCol-DestRow-DestCol) :-
    valid_position(Board, SourceRow-SourceCol),
    valid_position(Board, DestRow-DestCol),
    check_player_piece(Board, Player, SourceRow-SourceCol),
    is_empty(Board, DestRow-DestCol),
    check_larger_group(Board, SourceRow-SourceCol, DestRow-DestCol).


% TODO ...

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


game_loop(Board-Player) :-
    game_over(Board-Player, Winner),
    !,
    player_name(Winner, Name),
    write(Name),
    write(' won!\n').

game_loop(Board-Player) :-
    game_play(Board-Player, NewBoard-NewPlayer),
    game_loop(NewBoard-NewPlayer).


game_play(Board-Player, NewGameState) :-
    valid_moves(Board-Player, Player, ValidMoves),
    length(ValidMoves, N),
    N > 0,
    display_game(Board-Player),
    repeat,
    read_coordinates(SourceRow-SourceCol, DestRow-DestCol),
    move(Board-Player, SourceRow-SourceCol-DestRow-DestCol, NewGameState),
    !.

game_play(Board-Player, NewBoard-NewPlayer) :-
    valid_moves(Board-Player, Player, ValidMoves),
    length(ValidMoves, N),
    N =:= 0,
    display_game(Board-Player),
    repeat,
    read_coordinates(Row-Col),
    replace_piece(Board, empty, Row-Col, NewBoard),
    !,
    change_player(Player, NewPlayer).

% start_human_human/0
start_human_human :-
    get_color(Player),
    get_name(Player),
    change_player(Player, NewPlayer),
    get_name(NewPlayer).
