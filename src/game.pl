% game.pl


% player_attributes(?Name, ?Player, ?Difficulty)
% maps a player to its attributes (name and difficulty)
:- dynamic player_attributes/3.


% change_color(?Color, ?NewColor)
% changes the color
change_color(blue, red).
change_color(red, blue).


% change_player(?Player, ?NewPlayer)
% changes the player
change_player(1, 2).
change_player(2, 1).


% player_piece(?Player, ?Piece)
% maps a player to its pieces' color
player_piece(1, red).
player_piece(2, blue).


% replace_piece(+Board, +Piece, +Position, -NewBoard)
% replaces a given position on the board with a piece
replace_piece(Board, Piece, RowPos-ColPos, NewBoard) :-
    nth0(RowPos, Board, Row),
    replace(Row, Piece, ColPos, NewRow),
    replace(Board, NewRow, RowPos, NewBoard).


% move_piece(+Board, +Piece, +SourcePos, +DestPos, -NewBoard)
% moves a piece from one position on the board to another
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
% maps a piece to its position on the board and vice versa
piece(Board, Row-Col, Piece) :-
    nth0(Row, Board, BoardRow),
    nth0(Col, BoardRow, Piece).


% is_empty(?Board, ?Position)
% checks if a given position on the board is empty
is_empty(Board, Position) :-
    piece(Board, Position, empty).


% check_player_piece(+Board, +Player, +Position)
% checks if a given position on the board belongs to a specific player
check_player_piece(Board, Player, Position) :-
    piece(Board, Position, Piece),
    player_piece(Player, Piece).


% same_color(+Board, +Position1, +Position2)
% checks if two positions on the board have pieces of the same color
same_color(Board, Position1, Position2) :-
    piece(Board, Position1, Piece),
    piece(Board, Position2, Piece).


% neighbor(+Board, +Position, -NeighborPosition)
% given a position on the board, retrieves its neighbors' with pieces of the same color
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


% flood_fill(+Board, +ToFill, -Filled)
% uses the flood fill algorithm to find all connected positions of a given piece
flood_fill(Board, ToFill, Filled) :-
    flood_fill_aux(Board, ToFill, [], Filled).

flood_fill_aux(_, [], Filled, Filled).

flood_fill_aux(Board, [Position | T], Acc, Filled) :-
    findall(NeighborPosition, (neighbor(Board, Position, NeighborPosition), \+member(NeighborPosition, T), \+member(NeighborPosition, Acc)), Neighbors),
    append(T, Neighbors, ToVisit),
    flood_fill_aux(Board, ToVisit, [Position | Acc], Filled).


% check_larger_group(+Board, +SourcePos, +DestPos)
% checks if moving a piece from a source position on the board to a destination position results in a larger group of the same color
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


% all_positions(+GameState, -Positions)
% finds all pieces' positions for a given player on a board
all_positions(Board-Player, Positions) :-
    player_piece(Player, Piece),
    findall(Position, piece(Board, Position, Piece), Positions).


% all_groups(+Board, +Positions, -ListOfGroups)
% finds all groups of a given player from its pieces on a board
all_groups(Board, Positions, ListOfGroups) :-
    all_groups_aux(Board, Positions, [], ListOfGroups).

all_groups_aux(_, [], ListGroups, ListGroups).

all_groups_aux(Board, [H | T], Acc, ListOfGroups) :-
    flood_fill(Board, [H], Filled),
    sort(Filled, FilledSorted),
    append([FilledSorted], Acc, Acc1),
    all_groups_aux(Board, T, Acc1, ListOfGroups).


% biggest_group_length(+ListOfGroups, -MaxLength)
% gets the length of the biggest group of pieces
biggest_group_length([], 0).
biggest_group_length([H | T], MaxLength) :-
    biggest_group_length(T, TailMax),
    length(H, N),
    N > TailMax,
    MaxLength is N.
    
biggest_group_length([H | T], MaxLength) :-
    biggest_group_length(T, TailMax),
    length(H, N),
    N =< TailMax,
    MaxLength is TailMax.


% game_loop(+GameState)
% it is the main game loop
% if the game is over, it prints the name of the winner and ends the loop
game_loop(Board-Player) :-
    game_over(Board-Player, Winner),
    length(Board, BoardSize),
    print_header(0, BoardSize),
    print_board(Board, 0),
    print_board_line(BoardSize),
    player_attributes(Name, Winner, _),
    nl,
    write(Name),
    write(' won!\n').

% if the game is not over, it plays the game and continues the loop with a new game state
game_loop(GameState) :-
    game_play(GameState, NewGameState),
    game_loop(NewGameState).


% game_play(+GameState, -NewGameState)
% if there are valid moves, it displays the game, waits for the player to enter coordinates of a valid move,
% moves the piece and returns the new game state
game_play(Board-Player, NewGameState) :-
    valid_moves(Board-Player, Player, ValidMoves),
    length(ValidMoves, N),
    N > 0,
    display_game(Board-Player),
    repeat,
    move_coordinates(Board-Player, SourceRow-SourceCol, DestRow-DestCol),
    move(Board-Player, SourceRow-SourceCol-DestRow-DestCol, NewGameState),
    !.

% if there are no valid moves, it displays the game, waits for the player to enter coordinates of a valid piece,
% removes the piece at those coordinates and returns the new game state
game_play(Board-Player, NewBoard-NewPlayer) :-
    valid_moves(Board-Player, Player, ValidMoves),
    length(ValidMoves, N),
    N =:= 0,
    display_game(Board-Player),
    repeat,
    piece_coordinates(Board-Player, Row-Col),
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
    asserta(player_attributes(Name, Color, 0)),
    asserta(player_attributes(NewName, NewColor, 0)).


% start_human_bot/0
% starts a game between a human player and a bot
start_human_bot :-
    get_name(Name),
    get_color(Name, Color),
    change_player(Color, NewColor),
    asserta(player_attributes(Name, Color, 0)),
    bot_menu(Difficulty),
    bot_difficulty(Difficulty, BotName),
    asserta(player_attributes(BotName, NewColor, Difficulty)).


% start_bot_bot/0
% starts a game between two bots
start_bot_bot :-
    get_color('Bot 1', Color),
    change_player(Color, NewColor),
    bot_menu(Difficulty), 
    bot_difficulty(Difficulty, BotName),
    bot_color(Color, BotColor),
    atom_concat(BotName, BotColor, Name),
    asserta(player_attributes(Name, Color, Difficulty)),
    bot_menu(NewDifficulty),
    bot_difficulty(NewDifficulty, NewBotName),
    bot_color(NewColor, NewBotColor),
    atom_concat(NewBotName, NewBotColor, NewName),
    asserta(player_attributes(NewName, NewColor, NewDifficulty)).
