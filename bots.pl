:- use_module(library(random)).


%
bot_difficulty(1, 'Easy Bot').
bot_difficulty(2, 'Greedy Bot').
bot_difficulty(3, 'Hard Bot').

bot_color(1, ' (red)').
bot_color(2, ' (blue)').

% choose_move_easy(+Board-Player, -SourceRow-SourceCol-DestRow-DestCol)
% chooses a random valid move for an easy bot on a given board when there are valid moves
choose_move_easy(Board-Player, SourceRow-SourceCol-DestRow-DestCol):- 
    valid_moves(Board-Player, Player, ListOfMoves),
    random_member(SourceRow-SourceCol-DestRow-DestCol, ListOfMoves).


% choose_move_easy_NoValid(+Board-Player, -Row-Col)
% chooses a random valid move for a player on a given board when there are not valid moves
choose_move_easy_NoValid(Board-Player, Row-Col):- 
    player_piece(Player, Piece),
    findall(Row-Col, piece(Board, Row-Col,Piece), ListPositions),
    random_member(Row-Col, ListPositions).


% choose_move_hard(+Board-Player, -SourceRow-SourceCol-DestRow-DestCol)
% chooses a move for a hard bot on a given board when there are valid moves
choose_move_hard(Board-Player, SourceRow-SourceCol-DestRow-DestCol) :-
    valid_moves(Board-Player, Player, ValidMoves),
    hard_bot(Board-Player, ValidMoves, ListValues),
    find_max_value(ListValues, Index),
    nth0(Index, ValidMoves, SourceRow-SourceCol-DestRow-DestCol).


% choose_move_hard_NoValid(+Board-Player, -Row-Col)
% chooses a valid move for a hard bot on a given board when there are not valid moves
choose_move_hard_NoValid(Board-Player, Row-Col) :-
    player_piece(Player, Piece),
    findall(Row-Col, piece(Board, Row-Col,Piece), ListPositions),
    print(ListPositions),
    hard_bot_NoValid(Board-Player, ListPositions, ListValues),
    print(here),
    find_max_value(ListValues, Index),
    print(ListValues),
    print(Index),
    nth0(Index, ListPositions, Row-Col).


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


% greedy(+GameState, -Move)
% generates all possible next game states and uses the best/3 predicate to find the best one when there are valid moves
greedy(Board-Player, Move) :-
    valid_moves(Board-Player, Player, ValidMoves),
    next_boards(Board-Player, ValidMoves, NextGameStates),
    list_of_values(NextGameStates, ListValues),
    find_max_value(ListValues, Index),
    nth0(Index, NextGameStates, BestBoard-_),
    change_player(Player, NewPlayer),
    move(Board-Player, Move, BestBoard-NewPlayer).


% greedy_NoValid(+GameState, -Move)
% generates all possible next game states and uses the best/3 predicate to find the best one when there are not valid moves
greedy_NoValid(Board-Player, Move) :- %cut para se existirem varias boards e positions bons 
    player_piece(Player, Piece),
    findall(Row-Col, piece(Board, Row-Col,Piece), ListPositions),
    next_boards_NoValid(Board-Player, ListPositions, NextGameStates),
    list_of_values(NextGameStates, ListValues),
    find_max_value(ListValues, Index),
    nth0(Index, NextGameStates, BestBoard-_),
    replace_piece(Board, empty, Move, BestBoard).


% list_of_values(+NextGameStates, -ListValues)
% gets a list with the value for each NextGameStates
list_of_values([],[]).
list_of_values([Board-Player|T], [Value | ListValues]) :-
    value(Board-Player, Player, Value),
    list_of_values(T, ListValues).


% max_list_values(+List, -Max)
% finds out the the maximum value of a list
max_list_values([Max], Max).
max_list_values([H|T], H):-
    max_list_values(T, Max),
    H > Max.
max_list_values([H|T], Max):-
    max_list_values(T, Max),
    H =< Max.


% find_max_value(+ListValues, -Index)
% finds out the index of the maximum value of a list
find_max_value(ListValues, Index) :-
    max_list_values(ListValues, Max),
    nth0(Index, ListValues, Max).


% coordinates(+Board-Player, -SourceRow-SourceCol, -DestRow-DestCol)
% gets the input coordinates for an human player's next move when there are valid moves
move_coordinates(_-Player, SourceRow-SourceCol, DestRow-DestCol) :-
    player_name(_, Player , human),
    read_coordinates(SourceRow-SourceCol, DestRow-DestCol).


% coordinates(+Board-Player, -SourceRow-SourceCol, -DestRow-DestCol)
% gets the coordinates for an easy bot´s next move when there are valid moves
move_coordinates(Board-Player, SourceRow-SourceCol, DestRow-DestCol) :-
    player_name(_, Player , 1),
    choose_move_easy(Board-Player, SourceRow-SourceCol-DestRow-DestCol).


% coordinates(+Board-Player, -SourceRow-SourceCol, -DestRow-DestCol)
% gets the coordinates for a greedy bot´s next move when there are valid moves
move_coordinates(Board-Player, SourceRow-SourceCol, DestRow-DestCol) :-
    player_name(_, Player, 2),
    greedy(Board-Player, SourceRow-SourceCol-DestRow-DestCol).


% coordinates(+Board-Player, -SourceRow-SourceCol, -DestRow-DestCol)
% gets the coordinates for an hard bot´s next move when there are valid moves
move_coordinates(Board-Player, SourceRow-SourceCol, DestRow-DestCol) :-
    player_name(_, Player, 3),
    choose_move_hard(Board-Player, SourceRow-SourceCol-DestRow-DestCol).


% coordinates_NoValid(+Board-Player, -Row-Col)
% gets the input coordinates for an human player's next move when there are not valid moves
piece_coordinates(_-Player, Row-Col) :-
    player_name(_, Player , human),
    read_coordinates(Row-Col).


% coordinates_NoValid(+Board-Player, -Row-Col)
% gets the coordinates for an easy bot´s next move when there are not valid moves
piece_coordinates(Board-Player, Row-Col) :-
    player_name(_, Player , 1),
    choose_move_easy_NoValid(Board-Player, Row-Col).


%coordinates_NoValid(+Board-Player, -Row-Col)
% gets the coordinates for a greedy bot´s next move when there are not valid moves
piece_coordinates(Board-Player, Row-Col) :-
    player_name(_, Player, 2),
    greedy_NoValid(Board-Player, Row-Col).


%coordinates_NoValid(+Board-Player, -Row-Col)
% gets the coordinates for an hard bot´s next move when there are not valid moves
piece_coordinates(Board-Player, Row-Col) :-
    player_name(_, Player, 3),
    choose_move_hard_NoValid(Board-Player, Row-Col).


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


% value(+GameState, +Player, -Value)
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
    greedy(NewBoard-NewPlayer, MoveSourceRow-MoveSourceCol-MoveDestRow-MoveDestCol),
    !,
    player_piece(NewPlayer, NewPiece),
    move_piece(NewBoard, NewPiece, MoveSourceRow-MoveSourceCol, MoveDestRow-MoveDestCol, NewBoard1),
    value(NewBoard1-NewPlayer, NewPlayer, Value1),
    Value2 is Value - Value1,
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
    greedy_NoValid(NewBoard-NewPlayer, MoveSourceRow-MoveSourceCol),
    !,
    replace_piece(NewBoard, empty, MoveSourceRow-MoveSourceCol, NewBoard1),
    value(NewBoard1-NewPlayer, NewPlayer, Value1),
    Value2 is Value - Value1,
    hard_NoValid_aux(Board-Player, T, [Value2 |Acc], LValues).

hard_NoValid_aux(Board-Player, [Row-Col | T], Acc, LValues) :-
    replace_piece(Board, empty, Row-Col, NewBoard),
    value(NewBoard-Player, Player, Value),
    change_player(Player, NewPlayer),
    greedy(NewBoard-NewPlayer, MoveSourceRow-MoveSourceCol-MoveDestRow-MoveDestCol),
    !,
    player_piece(NewPlayer, NewPiece),
    move_piece(NewBoard, NewPiece, MoveSourceRow-MoveSourceCol, MoveDestRow-MoveDestCol, NewBoard1),
    value(NewBoard1-NewPlayer, NewPlayer, Value1),
    Value2 is Value - Value1,
    hard_NoValid_aux(Board-Player, T, [Value2 |Acc], LValues).

