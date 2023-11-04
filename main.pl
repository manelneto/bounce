% main.pl
% play
% display_game
% initial_state
% move
% valid_moves
% game_over
% value
% choose_move


:- consult(menu).
:- consult(game).
:- consult(bots).


% play/0
% initial state of the game
play :-
    bounce,
    initial_menu(GameMode),
    play(GameMode).


% play(+GameMode)
% initial state of the game human-human
play(1) :-
    start_human_human,
    board_menu(BoardSize),
    initial_state(BoardSize, GameState),
    game_loop(GameState),
    !,
    retractall(player_name(_, _, _)).


% play(+GameMode)
% initial state of the game human-bot
play(2) :- 
    start_human_bot,
    board_menu(BoardSize),
    initial_state(BoardSize, GameState),
    game_loop(GameState),
    !,
    retractall(player_name(_, _, _)).


% play(+GameMode)
% initial state of the game bot-bot
play(3) :- 
    start_bot_bot,
    board_menu(BoardSize),
    initial_state(BoardSize, GameState),
    game_loop(GameState),
    !,
    retractall(player_name(_, _, _)).


% display_game(+GameState)
% display the first board and turn of the game
display_game(Board-Player) :-
    length(Board, BoardSize),
    print_header(0, BoardSize),
    print_board(Board, 0),
    print_board_line(BoardSize),
    player_name(Name, Player, _),
    print_turn(Name).


% initial_state(+Size, -GameState)
% create the board with a BoardSize
initial_state(BoardSize, Board-1) :-
    create_board(BoardSize, Board).


% move(+GameState, +Move, -NewGameState).
% move a piece from source to destination coordinates of a given board and player
move(Board-Player, SourceRow-SourceCol-DestRow-DestCol, NewBoard-NewPlayer) :-
    can_move(Board-Player, SourceRow-SourceCol-DestRow-DestCol), % valid_moves(Board-Player, Player, ValidMoves), member(Move, ValidMoves),
    player_piece(Player, Piece),
    move_piece(Board, Piece, SourceRow-SourceCol, DestRow-DestCol, NewBoard),
    change_player(Player, NewPlayer).


% valid_moves(+GameState, +Player, -ListOfMoves)
% Gets all valid moves of a given board and player
valid_moves(Board-_, Player, ListOfMoves) :-
    findall(SourceRow-SourceCol-DestRow-DestCol, can_move(Board-Player, SourceRow-SourceCol-DestRow-DestCol), ListOfMoves).


% game_over(+GameState, -Winner)
% verify the game over
game_over(Board-Player, Winner) :-
    change_player(Player, Winner),
    player_piece(Winner, Piece),
    piece(Board, Position, Piece),
    !,
    flood_fill(Board, [Position], Group),
    player_pieces_number(Board, Piece, N),
    length(Group, N).


% choose_move(+GameState, +Player, +Level, -Move)