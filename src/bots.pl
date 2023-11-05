% bots.pl


% bot_difficulty(?Difficulty, ?Name)
% maps a bot's difficulty to its name
bot_difficulty(1, 'Easy Bot').
bot_difficulty(2, 'Greedy Bot').
bot_difficulty(3, 'Hard Bot').


% bot_color(?Bot, ?Color)
% maps a bot to its color
bot_color(1, ' (red)').
bot_color(2, ' (blue)').


% move_coordinates(+GameState -SourcePosition, -DestPosition)
% gets the input coordinates for a human's next move
move_coordinates(_-Player, SourcePosition, DestPosition) :-
    player_attributes(_, Player , 0),
    read_coordinates(SourcePosition, DestPosition).


% move_coordinates(+GameState -SourcePosition, -DestPosition)
% gets the coordinates for a bot's next move
move_coordinates(Board-Player, SourceRow-SourceCol, DestRow-DestCol) :-
    player_attributes(_, Player, Level),
    Level > 0,
    choose_move(Board-Player, Player, Level, SourceRow-SourceCol-DestRow-DestCol).


% piece_coordinates(+GameState, -Position)
% gets the input coordinates for a human's piece removal
piece_coordinates(_-Player, Position) :-
    player_attributes(_, Player , 0),
    read_coordinates(Position).


% piece_coordinates(+GameState, -Position)
% gets the input coordinates for a bot's piece removal
piece_coordinates(Board-Player, Position) :-
    player_attributes(_, Player , Level),
    Level > 0,
    choose_piece(Board-Player, Player, Level, Position).


% choose_move_easy(+GameState, -Move)
% chooses a random valid move for an easy bot
choose_move_easy(Board-Player, Move) :- 
    valid_moves(Board-Player, Player, ValidMoves),
    random_member(Move, ValidMoves).


% choose_move_greedy(+GameState, -Move)
% chooses a valid move for a greedy bot by generating all possible next game states and finding the best one
choose_move_greedy(Board-Player, Move) :-
    valid_moves(Board-Player, Player, ValidMoves),
    next_boards_after_move(Board-Player, ValidMoves, NextGameStates),
    list_of_values(NextGameStates, ListOfValues),
    max_value_index(ListOfValues, Index),
    nth0(Index, NextGameStates, BestBoard-_),
    change_player(Player, NewPlayer),
    move(Board-Player, Move, BestBoard-NewPlayer).


% choose_move_hard(+GameState, -Move)
% chooses a valid move for a hard bot
choose_move_hard(Board-Player, Move) :-
    valid_moves(Board-Player, Player, ValidMoves),
    hard_bot_move(Board-Player, ValidMoves, ListOfValues),
    max_value_index(ListOfValues, Index),
    nth0(Index, ValidMoves, Move).


% choose_piece_easy(+GameState, -Position)
% chooses a random piece's position for an easy bot to remove
choose_piece_easy(Board-Player, Position) :- 
    all_positions(Board-Player, ValidPositions),
    random_member(Position, ValidPositions).


% choose_piece_greedy(+GameState, -Position)
% chooses a piece's position for a greedy bot to remove by generating all possible next game states and finding the best one
choose_piece_greedy(Board-Player, Position) :-
    all_positions(Board-Player, ValidPositions),
    next_boards_after_removal(Board-Player, ValidPositions, NextGameStates),
    list_of_values(NextGameStates, ListOfValues),
    max_value_index(ListOfValues, Index),
    nth0(Index, NextGameStates, BestBoard-_),
    replace_piece(Board, empty, Position, BestBoard).


% choose_piece_hard(+GameState, -Position)
% chooses a piece's position for a hard bot to remove
choose_piece_hard(Board-Player, Position) :-
    all_positions(Board-Player, ValidPositions),
    hard_bot_removal(Board-Player, ValidPositions, ListOfValues),
    max_value_index(ListOfValues, Index),
    nth0(Index, ValidPositions, Position).


% next_boards_after_move(+GameState, +ValidMoves, -NextBoards)
% generates a list of all possible next boards given a list of valid moves
next_boards_after_move(Board-Player, ValidMoves, NextBoards) :-
    next_boards_after_move_aux(Board-Player, ValidMoves, [], NextBoards).

next_boards_after_move_aux(_, [], NextBoards, NextBoards).

next_boards_after_move_aux(Board-Player, [SourceRow-SourceCol-DestRow-DestCol | T], Acc, NextBoards) :-
    piece(Board, SourceRow-SourceCol, Piece),
    move_piece(Board, Piece, SourceRow-SourceCol, DestRow-DestCol, NewBoard),
    next_boards_after_move_aux(Board-Player, T, [NewBoard-Player | Acc], NextBoards).


% next_boards_after_removal(+GameState, +ValidPositions, -NextBoards)
% generates a list of all possible next boards given a list of valid removal positions
next_boards_after_removal(Board-Player, ValidPositions, NextBoards) :-
    next_boards_after_removal_aux(Board-Player, ValidPositions, [], NextBoards).

next_boards_after_removal_aux(_, [], NextBoards, NextBoards).

next_boards_after_removal_aux(Board-Player, [Row-Col | T], Acc, NextBoards) :-
    replace_piece(Board, empty, Row-Col, NewBoard),
    next_boards_after_removal_aux(Board-Player, T, [NewBoard-Player | Acc], NextBoards).


% list_of_values(+GameStates, -ListOfValues)
% gets a list with the value of each GameState
list_of_values([], []).

list_of_values([Board-Player | T], [Value | ListOfValues]) :-
    value(Board-Player, Player, Value),
    list_of_values(T, ListOfValues).


% hard_bot_move(+GameState, +ValidMoves, -ListOfValues)
% generates a list with the value of each move, according to the hard bot algorithm
hard_bot_move(Board-Player, ValidMoves, ListOfValues) :-
    hard_bot_move_values(Board-Player, ValidMoves, InvertedListOfValues),
    invert(InvertedListOfValues, ListOfValues).


% hard_bot_move_values(+GameState, +ValidMoves, -ListOfValues)
% calculates the list of values for each board with one play from player 1 and the next play from player 2 when there are valid moves
hard_bot_move_values(Board-Player, ValidMoves, ListOfValues) :-
    hard_bot_move_values_aux(Board-Player, ValidMoves, [], ListOfValues).

hard_bot_move_values_aux(_, [], ListOfValues, ListOfValues).

hard_bot_move_values_aux(Board-Player, [SourceRow-SourceCol-DestRow-DestCol | T], Acc, ListOfValues) :-
    player_piece(Player, Piece),
    move_piece(Board, Piece, SourceRow-SourceCol, DestRow-DestCol, NewBoard), 
    value(NewBoard-Player, Player, Value),
    change_player(Player, NewPlayer),
    choose_move_greedy(NewBoard-NewPlayer, MoveSourceRow-MoveSourceCol-MoveDestRow-MoveDestCol),
    !,
    player_piece(NewPlayer, NewPiece),
    move_piece(NewBoard, NewPiece, MoveSourceRow-MoveSourceCol, MoveDestRow-MoveDestCol, NewBoard1),
    value(NewBoard1-NewPlayer, NewPlayer, Value1),
    Value2 is (2 * Value) - Value1,
    hard_bot_move_values_aux(Board-Player, T, [Value2 | Acc], ListOfValues).
    

% hard_bot_removal(+GameState, +ValidPositions, -ListOfValues)
% generates a list with the value of each removal, according to the hard bot algorithm
hard_bot_removal(Board-Player, ValidPositions, ListOfValues) :-
    hard_bot_removal_values(Board-Player, ValidPositions, InvertedListOfValues),
    invert(InvertedListOfValues, ListOfValues).


% hard_bot_removal_values(+GameState, +ValidPositions, -ListOfValues)
% calculates the list of values for each board with one play from the player 1 and the next play from player 2 when there are no valid moves
hard_bot_removal_values(Board-Player, ValidPositions, ListOfValues) :-
    hard_bot_removal_values_aux(Board-Player, ValidPositions, [], ListOfValues).

hard_bot_removal_values_aux(_, [], ListOfValues, ListOfValues).

hard_bot_removal_values_aux(Board-Player, [Row-Col | T], Acc, ListOfValues) :-
    replace_piece(Board, empty, Row-Col, NewBoard),
    value(NewBoard-Player, Player, Value),
    change_player(Player, NewPlayer),
    valid_moves(NewBoard-NewPlayer, NewPlayer, NewValidMoves),
    length(NewValidMoves, N),
    N =:= 0,
    choose_piece_greedy(NewBoard-NewPlayer, MoveSourceRow-MoveSourceCol),
    !,
    replace_piece(NewBoard, empty, MoveSourceRow-MoveSourceCol, NewBoard1),
    value(NewBoard1-NewPlayer, NewPlayer, Value1),
    Value2 is (2 * Value) - Value1,
    hard_bot_removal_values_aux(Board-Player, T, [Value2 | Acc], ListOfValues).

hard_bot_removal_values_aux(Board-Player, [Row-Col | T], Acc, ListOfValues) :-
    replace_piece(Board, empty, Row-Col, NewBoard),
    value(NewBoard-Player, Player, Value),
    change_player(Player, NewPlayer),
    choose_move_greedy(NewBoard-NewPlayer, MoveSourceRow-MoveSourceCol-MoveDestRow-MoveDestCol),
    !,
    player_piece(NewPlayer, NewPiece),
    move_piece(NewBoard, NewPiece, MoveSourceRow-MoveSourceCol, MoveDestRow-MoveDestCol, NewBoard1),
    value(NewBoard1-NewPlayer, NewPlayer, Value1),
    Value2 is (2 * Value) - Value1,
    hard_bot_removal_values_aux(Board-Player, T, [Value2 | Acc], ListOfValues).
