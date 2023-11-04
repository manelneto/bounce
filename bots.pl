% bots.pl

% bot*
% *coordinates
% choose*


:- use_module(library(random)).


% bot_difficulty(?Difficulty, ?Name)
% maps a bot's difficulty to its name
bot_difficulty(1, 'Easy Bot').
bot_difficulty(2, 'Greedy Bot').
bot_difficulty(3, 'Hard Bot').

% bot_color(?Bot, ?Color)
% maps a bot's to its color
bot_color(1, ' (red)').
bot_color(2, ' (blue)').


% move_coordinates(+GameState -SourcePosition, -DestPosition)
% gets the input coordinates for a human's next move
move_coordinates(_-Player, SourcePosition, DestPosition) :-
    player_name(_, Player , 0),
    read_coordinates(SourcePosition, DestPosition).


% move_coordinates(+GameState -SourcePosition, -DestPosition)
% gets the coordinates for a bot's next move
move_coordinates(Board-Player, SourceRow-SourceCol, DestRow-DestCol) :-
    player_name(_, Player, Level),
    Level > 0,
    choose_move(Board-Player, Player, Level, SourceRow-SourceCol-DestRow-DestCol).


% piece_coordinates(+GameState, -Position)
% gets the input coordinates for a human's piece removal
piece_coordinates(_-Player, Position) :-
    player_name(_, Player , 0),
    read_coordinates(Position).


% piece_coordinates(+GameState, -Position)
% gets the input coordinates for a bot's piece removal
piece_coordinates(Board-Player, Position) :-
    player_name(_, Player , Level),
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
    next_boards(Board-Player, ValidMoves, NextGameStates),
    list_of_values(NextGameStates, Values),
    max_value_index(Values, Index),
    nth0(Index, NextGameStates, BestBoard-_),
    change_player(Player, NewPlayer),
    move(Board-Player, Move, BestBoard-NewPlayer).


% choose_move_hard(+GameState, -Move)
% chooses a valid move for a hard bot
choose_move_hard(Board-Player, Move) :-
    valid_moves(Board-Player, Player, ValidMoves),
    hard_bot(Board-Player, ValidMoves, Values),
    max_value_index(Values, Index),
    nth0(Index, ValidMoves, Move).


% choose_piece_easy(+GameState, -Position)
% chooses a random piece's position for an easy bot to remove
choose_piece_easy(Board-Player, Position) :- 
    player_piece(Player, Piece),
    findall(Row-Col, piece(Board, Row-Col, Piece), ValidPositions),
    random_member(Position, ValidPositions).


% choose_piece_greedy(+GameState, -Position)
% chooses a piece's position for a greedy bot to remove by generating all possible next game states and finding the best one
choose_piece_greedy(Board-Player, Position) :- % TODO - cut para se existirem varias boards e positions bons 
    player_piece(Player, Piece),
    findall(Row-Col, piece(Board, Row-Col, Piece), ValidPositions),
    next_boards_NoValid(Board-Player, ValidPositions, NextGameStates), % TODO
    list_of_values(NextGameStates, Values),
    max_value_index(Values, Index),
    nth0(Index, NextGameStates, BestBoard-_),
    replace_piece(Board, empty, Position, BestBoard).


% choose_piece_hard(+GameState, -Position)
% chooses a piece's position for a hard bot to remove
choose_piece_hard(Board-Player, Position) :-
    player_piece(Player, Piece),
    findall(Row-Col, piece(Board, Row-Col, Piece), ValidPositions),
    hard_bot_NoValid(Board-Player, ValidPositions, Values), % TODO
    max_value_index(Values, Index),
    nth0(Index, ValidPositions, Position).





























% next_boards(+Board-Player, +ValidMoves, -NextBoards)
% generates a list of all possible next boards given a list of valid moves
next_boards(Board-Player, ValidMoves, NextBoards) :-
    next_boards_aux(Board-Player, ValidMoves, [], NextBoards).

next_boards_aux(_, [], NextBoards, NextBoards).
next_boards_aux(Board-Player, [SourceRow-SourceCol-DestRow-DestCol | T], Acc, NextBoards) :-
    piece(Board, SourceRow-SourceCol, Piece),
    move_piece(Board, Piece, SourceRow-SourceCol, DestRow-DestCol, NewBoard),
    next_boards_aux(Board-Player, T, [NewBoard-Player | Acc], NextBoards).


% next_boards_NoValid(+Board-Player, +ListPositions, -NextBoards)
% generates a list of all possible next boards given a list of positions
next_boards_NoValid(Board-Player, ListPositions, NextBoards) :-
    next_boards_NoValid_aux(Board-Player, ListPositions, [], NextBoards).

next_boards_NoValid_aux(_, [], NextBoards, NextBoards).
next_boards_NoValid_aux(Board-Player, [Row-Col | T], Acc, NextBoards) :-
    replace_piece(Board, empty, Row-Col, NewBoard),
    next_boards_NoValid_aux(Board-Player, T, [NewBoard-Player | Acc], NextBoards).


% list_of_values(+NextGameStates, -ListValues)
% gets a list with the value for each NextGameStates
list_of_values([],[]).
list_of_values([Board-Player|T], [Value | ListValues]) :-
    value(Board-Player, Player, Value),
    list_of_values(T, ListValues).


% get_all_positions(+Board-Player, -Positions)
% gets all pieces positions of a given player and board
get_all_positions(Board-Player, Positions) :-
    player_piece(Player, Piece),
    findall(Row-Col, piece(Board, Row-Col, Piece), Positions).


% list_groups(+Board-Player, +Positions, -ListGroups)
% gets all groups of pieces of a given player using his pieces positions
list_groups(Board-Player, Positions, ListGroups) :-
    list_groups_aux(Board-Player, Positions, [], ListGroups).

list_groups_aux(_, [], ListGroups, ListGroups).
list_groups_aux(Board-Player, [H|T], Acc, ListGroups) :-
    flood_fill(Board,[H], Filled),
    sort(Filled, FilledSorted),
    append([FilledSorted], Acc, Acc1),
    list_groups_aux(Board-Player, T, Acc1, ListGroups).


% length_of_bigger_group(+SortedListGroups, -BiggerGroup)
% gets the length of the biggest group of pieces
length_of_bigger_group([],0).
length_of_bigger_group([H|T], BiggerGroup) :-
    length_of_bigger_group(T, Max),
    length(H, N),
    N > Max,
    BiggerGroup is N.
    
length_of_bigger_group([H|T], BiggerGroup) :-
    length_of_bigger_group(T, Max),
    length(H, N),
    N =< Max,
    BiggerGroup is Max.


% hard_bot(+Board-Player, +ValidMoves, -ListValues)
% receives a list of values and invert it 
hard_bot(Board-Player, ValidMoves, ListValues) :-
    hard(Board-Player, ValidMoves, LValues),
    invert(LValues, ListValues).


% hard(+Board-Player, +ValidMoves, -LValues)
% calculates the list of values for each board with one play from the player 1 and 2 when there are valid moves
hard(Board-Player, ValidMoves, LValues) :-
    hard_aux(Board-Player, ValidMoves, [], LValues).

hard_aux(_,[],LValues, LValues).
hard_aux(Board-Player, [SourceRow-SourceCol-DestRow-DestCol | T], Acc, LValues) :-
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
    hard_aux(Board-Player, T, [Value2 |Acc], LValues).
    

% hard_bot_NoValid(+Board-Player, +ValidMoves, -ListValues)
% receives a list of values and invert it 
hard_bot_NoValid(Board-Player, ListPositions, ListValues) :-
    hard_NoValid(Board-Player, ListPositions, LValues),
    invert(LValues, ListValues).


% hard(+Board-Player, +ListPositions, -LValues)
% calculates the list of values for each board with one play from the player 1 and 2 when there are not valid moves
hard_NoValid(Board-Player, ListPositions, LValues) :-
    hard_NoValid_aux(Board-Player, ListPositions, [], LValues).

hard_NoValid_aux(_,[],LValues, LValues).
hard_NoValid_aux(Board-Player, [Row-Col | T], Acc, LValues) :-
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
    hard_NoValid_aux(Board-Player, T, [Value2 |Acc], LValues).

hard_NoValid_aux(Board-Player, [Row-Col | T], Acc, LValues) :-
    replace_piece(Board, empty, Row-Col, NewBoard),
    value(NewBoard-Player, Player, Value),
    change_player(Player, NewPlayer),
    choose_move_greedy(NewBoard-NewPlayer, MoveSourceRow-MoveSourceCol-MoveDestRow-MoveDestCol),
    !,
    player_piece(NewPlayer, NewPiece),
    move_piece(NewBoard, NewPiece, MoveSourceRow-MoveSourceCol, MoveDestRow-MoveDestCol, NewBoard1),
    value(NewBoard1-NewPlayer, NewPlayer, Value1),
    Value2 is (2 * Value) - Value1,
    hard_NoValid_aux(Board-Player, T, [Value2 |Acc], LValues).
