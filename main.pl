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
play :-
    bounce,
    initial_menu(GameMode),
    play(GameMode).


play_random_bot :-
    bounce,
    get_color(Player),
    get_name(Player),
    change_player(Player, NewPlayer),
    get_name(NewPlayer),
    board_menu(BoardSize),
    initial_state(BoardSize, GameState),
    !,
    game_loop(GameState).


% play(+GameMode)
play(1) :-
    start_human_human,
    board_menu(BoardSize),
    initial_state(BoardSize, GameState),
    !,
    game_loop(GameState).

play(2) :- write('TODO').

play(3) :- write('TODO').


% display_game(+GameState)
display_game(Board-Player) :-
    length(Board, BoardSize),
    print_header(0, BoardSize),
    print_board(Board, 0),
    print_board_line(BoardSize),
    player_name(Player, Name),
    print_turn(Name).


% initial_state(+Size, -GameState)
initial_state(BoardSize, Board-1) :-
    create_board(BoardSize, Board).


% move(+GameState, +Move, -NewGameState).
move(Board-Player, SourceRow-SourceCol-DestRow-DestCol, NewBoard-NewPlayer) :-
    can_move(Board-Player, SourceRow-SourceCol-DestRow-DestCol), % valid_moves(Board-Player, Player, ValidMoves), member(Move, ValidMoves),
    player_piece(Player, Piece),
    move_piece(Board, Piece, SourceRow-SourceCol, DestRow-DestCol, NewBoard),
    change_player(Player, NewPlayer).


% valid_moves(+GameState, +Player, -ListOfMoves).
valid_moves(Board-_, Player, ListOfMoves) :-
    findall(SourceRow-SourceCol-DestRow-DestCol, can_move(Board-Player, SourceRow-SourceCol-DestRow-DestCol), ListOfMoves).


% game_over(+GameState, -Winner)
game_over(Board-Player, Winner) :-
    change_player(Player, Winner),
    player_piece(Winner, Piece),
    piece(Board, Position, Piece),
    !,
    flood_fill(Board, [Position], Group),
    player_pieces_number(Board, Piece, N),
    length(Group, N).


% value(+GameState, +Player, -Value)


% choose_move(+GameState, +Player, +Level, -Move)