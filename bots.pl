
choose_move_easy(Board-Player, SourceRow-SourceCol-DestRow-DestCol):- 
    valid_moves(Board-Player, ListOfMoves),
    random_member(SourceRow-SourceCol-DestRow-DestCol, ListOfMoves).
/*
choose_move_hard(SourceRow-SourceCol-DestRow-DestCol) :-
    valid_moves()
*/
/*
coordinates(SourceRow-SourceCol, DestRow-DestCol) :-
    player_name(Player, Name, human),
    read_coordinates(SourceRow-SourceCol, DestRow-DestCol).

coordinates(SourceRow-SourceCol, DestRow-DestCol) :-
    player_name(Player, Name, easy),
    choose_move_easy(SourceRow-SourceCol-DestRow-DestCol).

coordinates(SourceRow-SourceCol, DestRow-DestCol) :-
    player_name(Player, Name, hard),
    choose_move_hard(SourceRow-SourceCol-DestRow-DestCol).
*/

get_all_positions(Board-Player, Positions) :-
    player_color(Player, Piece),
    findall(Row-Col, get_piece_position(Board, Piece, Row-Col), Positions).

list_groups(Board-Player, Positions, ListGroups) :-
    list_groups_aux(Board-Player, Positions, [], ListGroups).

list_groups_aux(_, [], ListGroups, ListGroups).
list_groups_aux(Board-Player, [H|T], Acc, ListGroups) :-
    flood_fill(Board,[H], Filled),
    sort(Filled, FilledSorted),
    append([FilledSorted], Acc, Acc1),
    list_groups_aux(Board-Player, T, Acc1, ListGroups).

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
value(Board-Player, Player, Value) :-
    player_color(Player, Piece),
    player_pieces_number(Board, Piece, NumberPieces),
    get_all_positions(Board-Player, Positions),
    list_groups(Board-Player, Positions, ListGroups),
    sort(ListGroups, SortedListGroups),
    length(SortedListGroups, NumberGroups),
    length_of_bigger_group(SortedListGroups, BiggerGroup),
    Value is (NumberPieces * -0.25) + (NumberGroups * -0.65) + (BiggerGroup * 0.10).