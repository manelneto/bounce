% game.pl


:- consult(utils).


% change_color(?Color1, ?Color2)
% changes the color of a piece
change_color(blue, red).
change_color(red, blue).


% change_player(?Player, ?NewPlayer)
% changes the player
change_player(1, 2).
change_player(2, 1).


% player_piece(?Player, ?Piece)
% maps a player to a piece
player_piece(1, red).
player_piece(2, blue).


% replace_piece(+Board, +Piece, +Position, -NewBoard)
% replaces a piece at a given position on the board with a new piece
replace_piece(Board, Piece, RowPos-ColPos, NewBoard) :-
    nth0(RowPos, Board, Row),
    replace(Row, Piece, ColPos, NewRow),
    replace(Board, NewRow, RowPos, NewBoard).


% move_piece(+Board, +Piece, +SourcePos, +DestPos, -NewBoard)
% moves a piece from one position to another on the board
move_piece(Board, Piece, SourcePos, DestPos, NewBoard) :-
    replace_piece(Board, empty, SourcePos, _B),
    replace_piece(_B, Piece, DestPos, NewBoard).


% create_row(+RowSize, +CurrentColor, -Row)
% creates a row of a given size with alternating colors
create_row(RowSize, CurrentColor, Row) :-
    create_row_aux(RowSize, CurrentColor, [], Row).

create_row_aux(0, _, Row, Row).

create_row_aux(N, CurrentColor, Acc, Row) :-
    N > 0,
    N1 is N - 1,
    change_color(CurrentColor, NextColor),
    create_row_aux(N1, NextColor, [CurrentColor | Acc], Row).


% create_board(+BoardSize, -Board)
% creates a board of a given size
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
% checks if a given position is valid on the board
valid_position(Board, RowPos-ColPos) :-
    length(Board, BoardSize),
    N is BoardSize - 1,
    between(0, N, RowPos),
    between(0, N, ColPos).


% up(+Board, +Position, -UpPosition)
% calculates the position above a given position
up(Board, Row-Col, UpRow-Col) :-
    UpRow is Row - 1,
    valid_position(Board, UpRow-Col).


% down(+Board, +Position, -DownPosition)
% calculates the position below a given position
down(Board, Row-Col, DownRow-Col) :-
    DownRow is Row + 1,
    valid_position(Board, DownRow-Col).


% left(+Board, +Position, -LeftPosition)
% calculates the position to the left of a given position
left(Board, Row-Col, Row-LeftCol) :-
    LeftCol is Col - 1,
    valid_position(Board, Row-LeftCol).


% right(+Board, +Position, -RightPosition)
% calculates the position to the right of a given position
right(Board, Row-Col, Row-RightCol) :-
    RightCol is Col + 1,
    valid_position(Board, Row-RightCol).


% piece(?Board, ?Position, ?Piece)
% retrieves the piece at a given position on the board
piece(Board, Row-Col, Piece) :-
    nth0(Row, Board, BoardRow),
    nth0(Col, BoardRow, Piece).


% empty(?Board, ?Position)
% checks if a given position on the board is empty
is_empty(Board, Position) :-
    piece(Board, Position, empty).


% check_player_piece(+Board, +Player, +Position)
% checks if a given position on the board belongs to a specific player
check_player_piece(Board, Player, Position) :-
    piece(Board, Position, Piece),
    player_piece(Player, Piece).


% same_color(+Board, +Position1, +Position2)
% checks if a given position has a piece of the same color
same_color(Board, Position1, Position2) :-
    piece(Board, Position1, Piece),
    piece(Board, Position2, Piece).


% neighbor(+Board, +Position, -NeighborPosition)
% checks if a given position has a neighboring piece of the same color
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
% uses the flood fill algorithm to find all connected positions of a given piece
flood_fill(Board, ToFill, Filled) :-
    flood_fill_aux(Board, ToFill, [], Filled).

flood_fill_aux(_, [], Filled, Filled).

flood_fill_aux(Board, [Position | T], Acc, Filled) :-
    findall(NeighborPosition, (neighbor(Board, Position, NeighborPosition), \+member(NeighborPosition, T), \+member(NeighborPosition, Acc)), Neighbors),
    append(T, Neighbors, ToVisit),
    flood_fill_aux(Board, ToVisit, [Position | Acc], Filled).


% check_larger_group(+Board, +SourcePos, +DestPos)
% checks if moving a piece from a source position to a destination position results in a larger group of the same color
check_larger_group(Board, SourcePos, DestPos) :-
    flood_fill(Board, [SourcePos], SourceFilled),
    length(SourceFilled, SourceSize),
    piece(Board, SourcePos, Piece),
    move_piece(Board, Piece, SourcePos, DestPos, NewBoard),
    flood_fill(NewBoard, [DestPos], DestFilled),
    length(DestFilled, DestSize),
    DestSize > SourceSize.


% can_move(+GameState, +Move)
% checks if a given move is valid
can_move(Board-Player, SourceRow-SourceCol-DestRow-DestCol) :-
    valid_position(Board, SourceRow-SourceCol),
    valid_position(Board, DestRow-DestCol),
    check_player_piece(Board, Player, SourceRow-SourceCol),
    is_empty(Board, DestRow-DestCol),
    check_larger_group(Board, SourceRow-SourceCol, DestRow-DestCol).


% find_all_positions(+Board, +Piece, -ListPositions)
% finds all positions of a given piece on the board
find_all_positions(Board, Piece, ListPositions) :-
    findall(Row-Col, piece(Board, Row-Col,Piece), ListPositions).


% TODO ...

% player_pieces_number(+Board, +Piece, -N)
% counts the number of a given piece on the board
player_pieces_number(Board, Piece, N) :-
    findall(Row-Col, piece(Board, Row-Col,Piece), ListPositions),
    length(ListPositions, N).


% game_loop(+Board-Player)
% it is main game loop
% if the game is over, it prints the name of the winner and ends the loop
game_loop(Board-Player) :-
    game_over(Board-Player, Winner),
    player_name(Name, Winner, _),
    write(Name),
    write(' won!\n').

% if the game is not over, it plays the game and continues the loop with the new game state
game_loop(Board-Player) :-
    game_play(Board-Player, NewBoard-NewPlayer),
    game_loop(NewBoard-NewPlayer).


% game_play(+Board-Player, -NewGameState)
% if there are valid moves, it displays the game, waits for the player to enter coordinates,
% moves the piece, and returns the new game state
game_play(Board-Player, NewGameState) :-
    valid_moves(Board-Player, Player, ValidMoves),
    length(ValidMoves, N),
    N > 0,
    display_game(Board-Player),
    repeat,
    coordinates(Board-Player, SourceRow-SourceCol, DestRow-DestCol),
    move(Board-Player, SourceRow-SourceCol-DestRow-DestCol, NewGameState),
    !.

% if there are no valid moves, it displays the game, waits for the player to enter coordinates,
% replaces the piece at the entered coordinates with an empty piece, returns the new game state and changes the player.
game_play(Board-Player, NewBoard-NewPlayer) :-
    valid_moves(Board-Player, Player, ValidMoves),
    length(ValidMoves, N),
    N =:= 0,
    display_game(Board-Player),
    repeat,
    coordinates_NoValid(Board-Player, Row-Col),
    replace_piece(Board, empty, Row-Col, NewBoard),
    !,
    change_player(Player, NewPlayer).


% start_human_human/0
% starts a game between two human players
start_human_human :-
    get_name(Name),
    get_color(Name, Color),
    change_player(Color, NewColor),
    get_name(NewName),
    asserta(player_name(Name, Color, human)),
    asserta(player_name(NewName, NewColor, human)).


% start_human_bot/0
% starts a game between a human player and a bot
start_human_bot :-
    get_name(Name),
    get_color(Name, Color),
    change_player(Color, NewColor),
    asserta(player_name(Name, Color, human)),
    bot_menu(Difficulty),
    asserta(player_name('bot', NewColor, Difficulty)).


% start_bot_bot/0
% starts a game between two bots
start_bot_bot :-
    get_color('BOT 1', Color),
    change_player(Color, NewColor),
    bot_menu(Difficulty1), 
    asserta(player_name('BOT 1', Color, Difficulty1)),
    bot_menu(Difficulty2),
    asserta(player_name('BOT 2', NewColor, Difficulty2)).