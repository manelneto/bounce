% read_number(-X).
read_number(X) :-
    read_number_aux(0, false, X).

read_number_aux(Acc, _, X) :-
    get_code(C),
    C >= 48,
    C =< 57,
    !,
    Acc1 is 10 * Acc + (C - 48),
    read_number_aux(Acc1, true, X).

read_number_aux(X, true, X).


replace_piece(Piece, Board, Colpos, Rowpos, Res) :-
    nth0(Rowpos, Board, Row),
    replace_col_pos(Piece, Colpos, Row, NewRow), %susbtituir a piece
    replace_in_list(Rowpos, NewRow, Board, Res). %susbtituir a linha pela nova com a peça substituida

replace_col_pos(Piece, Colpos, Row, Res) :-
    nth0(Colpos, Row, _, R),
    nth0(Colpos, Res, Piece, R).

replace_in_list(Rowpos, Row, OldList, NewList) :-
    nth0(Rowpos, OldList, _, Temp),
    nth0(Rowpos, NewList, Row, Temp).