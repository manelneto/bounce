:- consult(menu).
:- use_module(library(lists)).

create_row(N, List,CurrentChar) :-
    create_row_aux(N, [], List, CurrentChar).

create_row_aux(0, List, List, _).
create_row_aux(N, Acc, List, CurrentChar) :-
    N > 0,
    N1 is N - 1,
    switch_char(CurrentChar, NextChar),
    create_row_aux(N1, [CurrentChar|Acc], List, NextChar).

switch_char('R', 'B').
switch_char('B', 'R').

create_board(N, Listoflists) :-
    create_row(N, Row, CurrentChar), %<--
    create_board_aux(N, Row, [], Listoflists).

create_board_aux(0, _, List, List).
create_board_aux(N, Row, Acc, List) :-
    N > 0,
    N1 is N - 1,
    create_board_aux(N1, Row, [Row | Acc], List).


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


create_all_board(N) :-
    create_board(N, B),
    N1 is N - 1,
    replace_piece('E', B, 0, 0, B1),
    replace_piece('E', B1, N1, 0, B2),
    replace_piece('E', B2, 0, N1, B3),
    replace_piece('E', B3, N1, N1, Board),
    print_board(Board).
