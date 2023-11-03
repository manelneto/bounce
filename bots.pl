:- use_module(library(random)).


% choose_move_easy(+Board-Player, -SourceRow-SourceCol-DestRow-DestCol)
% chooses a random valid move for a player on a given board when there are valid moves
choose_move_easy(Board-Player, SourceRow-SourceCol-DestRow-DestCol):- 
    valid_moves(Board-Player, Player, ListOfMoves),
    random_member(SourceRow-SourceCol-DestRow-DestCol, ListOfMoves).


% choose_move_easy_NoValid(+Board-Player, -Row-Col)
% chooses a random valid move for a player on a given board when there are not valid moves
choose_move_easy_NoValid(Board-Player, Row-Col):- 
    player_piece(Player, Piece),
    findall(Row-Col, piece(Board, Row-Col,Piece), ListPositions),
    random_member(Row-Col, ListPositions).


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


% minimax(+GameState, -BestNextGameState, +Player)
% generates all possible next game states and uses the best/3 predicate to find the best one when there are valid moves
minimax(Board-Player, Move) :-
    valid_moves(Board-Player, Player, ValidMoves),
    next_boards(Board-Player, ValidMoves, NextGameStates),
    list_of_values(NextGameStates, ListValues),
    find_max_value(ListValues, Index),
    nth0(Index, NextGameStates, BestBoard-_),
    change_player(Player, NewPlayer),
    move(Board-Player, Move, BestBoard-NewPlayer).


% minimax(+GameState, -BestNextGameState, +Player)
% generates all possible next game states and uses the best/3 predicate to find the best one when there are not valid moves
minimax_NoValid(Board-Player, Move) :- %cut para se existirem varias boards e positions bons 
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
coordinates(_-Player, SourceRow-SourceCol, DestRow-DestCol) :-
    player_name(_, Player , human),
    read_coordinates(SourceRow-SourceCol, DestRow-DestCol).


% coordinates(+Board-Player, -SourceRow-SourceCol, -DestRow-DestCol)
% gets the coordinates for an easy bot´s next move when there are valid moves
coordinates(Board-Player, SourceRow-SourceCol, DestRow-DestCol) :-
    player_name(_, Player , 1),
    choose_move_easy(Board-Player, SourceRow-SourceCol-DestRow-DestCol).


% coordinates(+Board-Player, -SourceRow-SourceCol, -DestRow-DestCol)
% gets the coordinates for an hard bot´s next move when there are valid moves
coordinates(Board-Player, SourceRow-SourceCol, DestRow-DestCol) :-
    player_name(_, Player, 2),
    minimax(Board-Player, SourceRow-SourceCol-DestRow-DestCol).


% coordinates_NoValid(+Board-Player, -Row-Col)
% gets the input coordinates for an human player's next move when there are not valid moves
coordinates_NoValid(_-Player, Row-Col) :-
    player_name(_, Player , human),
    read_coordinates(Row-Col).


% coordinates_NoValid(+Board-Player, -Row-Col)
% gets the coordinates for an easy bot´s next move when there are not valid moves
coordinates_NoValid(Board-Player, Row-Col) :-
    player_name(_, Player , 1),
    choose_move_easy_NoValid(Board-Player, Row-Col).


%coordinates_NoValid(+Board-Player, -Row-Col)
% gets the coordinates for an hard bot´s next move when there are not valid moves
coordinates_NoValid(Board-Player, Row-Col) :-
    player_name(_, Player, 2),
    minimax_NoValid(Board-Player, Row-Col).


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



