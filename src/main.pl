% main.pl


:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(random)).


:- consult(utils).
:- consult(menu).
:- consult(game).
:- consult(bots).


% play/0
% plays the game
play :-
    bounce,
    initial_menu(GameMode),
    play(GameMode),
    retractall(player_attributes(_, _, _)).


% play(+GameMode)
% plays a game of a given mode
play(1) :-
    start_human_human,
    board_menu(BoardSize),
    initial_state(BoardSize, Board-Player),
    game_loop(Board-Player),
    !.

play(2) :- 
    start_human_bot,
    board_menu(BoardSize),
    initial_state(BoardSize, Board-Player),
    game_loop(Board-Player),
    !.

play(3) :- 
    start_bot_bot,
    board_menu(BoardSize),
    initial_state(BoardSize, Board-Player),
    game_loop(Board-Player),
    !.


% display_game(+GameState)
% displays the board and the turn of the game
display_game(Board-Player) :-
    length(Board, BoardSize),
    print_header(0, BoardSize),
    print_board(Board, 0),
    print_board_line(BoardSize),
    player_attributes(Name, Player, _),
    print_turn(Name).


% initial_state(+BoardSize, -GameState)
% creates a board with a given size and returns the initial game state
initial_state(BoardSize, Board-1) :-
    create_board(BoardSize, Board).


% move(+GameState, +Move, -NewGameState).
% validates and executes a move
move(Board-Player, SourceRow-SourceCol-DestRow-DestCol, NewBoard-NewPlayer) :-
    can_move(Board-Player, SourceRow-SourceCol-DestRow-DestCol),
    player_piece(Player, Piece),
    move_piece(Board, Piece, SourceRow-SourceCol, DestRow-DestCol, NewBoard),
    change_player(Player, NewPlayer).


% valid_moves(+GameState, +Player, -ListOfMoves)
% obtains all valid moves for a given board and player
valid_moves(Board-_, Player, ListOfMoves) :-
    findall(SourceRow-SourceCol-DestRow-DestCol, can_move(Board-Player, SourceRow-SourceCol-DestRow-DestCol), ListOfMoves).


% game_over(+GameState, -Winner)
% verifies the end of the game and identifies the winner
game_over(Board-Player, Winner) :-
    change_player(Player, Winner),
    player_piece(Winner, Piece),
    piece(Board, Position, Piece),
    !,
    flood_fill(Board, [Position], Group),
    all_positions(Board-Winner, Positions),
    length(Positions, N),
    length(Group, N).

% value(+GameState, +Player, -Value)
% calculates the board value using the number of pieces, groups and the length of the biggest group of a given player
value(Board-Player, Player, Value) :-
    all_positions(Board-Player, Positions),
    length(Positions, NumberPieces),
    all_groups(Board, Positions, ListGroups),
    sort(ListGroups, SortedListGroups),
    length(SortedListGroups, NumberGroups),
    biggest_group_length(SortedListGroups, BiggerGroup),
    Value is (NumberPieces * -0.15) + (NumberGroups * -0.55) + (BiggerGroup * 0.3).


% choose_move(+GameState, +Player, +Level, -Move)
% chooses a move for the bot, depending on the level of difficulty
choose_move(Board-Player, Player, 1, Move) :-
    press_enter,
    choose_move_easy(Board-Player, Move),
    print_move(Move).

choose_move(Board-Player, Player, 2, Move) :-
    press_enter,
    choose_move_greedy(Board-Player, Move),
    print_move(Move).

choose_move(Board-Player, Player, 3, Move) :-
    press_enter,
    choose_move_hard(Board-Player, Move),
    print_move(Move).


% choose_piece(+GameState, +Player, +Level, -Position)
% chooses a piece's position for the bot, depending on the level of difficulty
choose_piece(Board-Player, Player, 1, Position) :-
    press_enter,
    choose_piece_easy(Board-Player, Position),
    print_piece(Position).

choose_piece(Board-Player, Player, 2, Position) :-
    press_enter,
    choose_piece_greedy(Board-Player, Position),
    print_piece(Position).

choose_piece(Board-Player, Player, 3, Position) :-
    press_enter,
    choose_piece_hard(Board-Player, Position),
    print_piece(Position).
