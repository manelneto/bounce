% main.pl

% play
% display_game
% initial_state
% move
% valid_moves
% game_over
% value
% choose_move
% choose_piece


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
    retractall(player_name(_, _, _)).


% play(+GameMode)
% plays a human-human game
play(1) :-
    start_human_human,
    board_menu(BoardSize),
    initial_state(BoardSize, GameState),
    game_loop(GameState),
    !.


% play(+GameMode)
% plays a human-bot game
play(2) :- 
    start_human_bot,
    board_menu(BoardSize),
    initial_state(BoardSize, GameState),
    game_loop(GameState),
    !.


% play(+GameMode)
% plays a bot-bot game
play(3) :- 
    start_bot_bot,
    board_menu(BoardSize),
    initial_state(BoardSize, GameState),
    game_loop(GameState),
    !.


% display_game(+GameState)
% displays the board and the turn of the game
display_game(Board-Player) :-
    length(Board, BoardSize),
    print_header(0, BoardSize),
    print_board(Board, 0),
    print_board_line(BoardSize),
    player_name(Name, Player, _),
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
    player_pieces_number(Board, Piece, N),
    length(Group, N).

% value(+GameState, +Player, -Value) - TODO
% calculates the board value using the number of pieces, groups and the length of the biggest group of a given player
value(Board-Player, Player, Value) :-
    player_piece(Player, Piece),
    player_pieces_number(Board, Piece, NumberPieces),
    get_all_positions(Board-Player, Positions),
    list_groups(Board-Player, Positions, ListGroups),
    sort(ListGroups, SortedListGroups),
    length(SortedListGroups, NumberGroups),
    length_of_bigger_group(SortedListGroups, BiggerGroup),
    Value is (NumberPieces * -0.25) + (NumberGroups * -0.65) + (BiggerGroup * 0.10).


% choose_move(+GameState, +Player, +Level, -Move)
% chooses a move for the bot, depending on the level of difficulty
choose_move(Board-Player, Player, 1, Move) :-
    choose_move_easy(Board-Player, Move),
    print_move(Move).

choose_move(Board-Player, Player, 2, Move) :-
    choose_move_greedy(Board-Player, Move),
    print_move(Move).

choose_move(Board-Player, Player, 3, Move) :-
    choose_move_hard(Board-Player, Move),
    print_move(Move).


% choose_piece(+GameState, +Player, +Level, -Position)
% chooses a piece's position for the bot, depending on the level of difficulty
choose_piece(Board-Player, Player, 1, Position) :-
    choose_piece_easy(Board-Player, Position),
    print_piece(Position).

choose_piece(Board-Player, Player, 2, Position) :-
    choose_piece_greedy(Board-Player, Position),
    print_piece(Position).

choose_piece(Board-Player, Player, 3, Position) :-
    choose_piece_hard(Board-Player, Position),
    print_piece(Position).
