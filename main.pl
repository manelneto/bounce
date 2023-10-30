:- consult(menu).
:- consult(plays).
:- use_module(library(lists)).

play :-
    bounce,
    board_menu(BoardSize),
    create_all_board(BoardSize, Board),
    game_loop(Board).

create_row(N, List,CurrentChar) :-
    create_row_aux(N, [], List, CurrentChar).

create_row_aux(0, List, List, _).
create_row_aux(N, Acc, List, CurrentChar) :-
    N > 0,
    N1 is N - 1,
    switch_char(CurrentChar, NextChar),
    create_row_aux(N1, [CurrentChar|Acc], List, NextChar).

switch_char('R', 'b').
switch_char('b', 'R').

create_board(N, Listoflists) :-
    create_board_aux(N, 0, [], Listoflists).

create_board_aux(N, Counter, Acc, List) :-
    Counter mod 2 =:= 0,
    create_row(N, Row, 'R'),
    Counter < N - 1,
    create_board_aux(N, Counter + 1, [Row | Acc], List).

create_board_aux(N, Counter, Acc, List) :-
    Counter mod 2 =:= 0,
    create_row(N, Row, 'R'),
    Counter >= N - 1,
    List = [Row | Acc].

create_board_aux(N, Counter, Acc, List) :-
    Counter mod 2 =\= 0,
    create_row(N, Row, 'b'),
    Counter < N - 1,
    create_board_aux(N, Counter + 1, [Row | Acc], List).

create_board_aux(N, Counter, Acc, List) :-
    Counter mod 2 =\= 0,
    create_row(N, Row, 'b'),
    Counter >= N - 1,
    List = [Row | Acc].

create_all_board(N, Board) :-
    create_board(N, B),
    N1 is N - 1,
    replace_piece('.', B, 0, 0, B1),
    replace_piece('.', B1, N1, 0, B2),
    replace_piece('.', B2, 0, N1, B3),
    replace_piece('.', B3, N1, N1, Board),
    print_board(Board).

print_row([]) :- nl, nl.
print_row([H|T]) :-
    write(H), write('  '),
    print_row(T).

print_board([]).
print_board([H|T]) :-
    print_row(H),
    print_board(T).


complete_play(Board, BoardNew, Piece, ColposToMove, RowToMove, ColposToPlace, RowposToPlace) :-
    replace_piece('.', Board, ColposToMove, RowToMove, B1),
    replace_piece(Piece, B1, ColposToPlace, RowposToPlace, BoardNew).


game_loop(Board) :-
    coordinates(ColposToMove, RowToMove, ColposToPlace, RowToPlace),
    complete_play(Board, BoardNew, 'b', ColposToMove, RowToMove, ColposToPlace, RowToPlace),
    print_board(BoardNew).



