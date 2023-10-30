:- consult(menu).
:- consult(game).

% play/0
play :-
    bounce,
    board_menu(BoardSize),
    create_all_board(BoardSize, Board),
    game_loop(Board-1).

% switch_char(?Char1, ?Char2)
switch_char(blue, red).
switch_char(red, blue).

% create_row(+RowSize, +CurrentChar, -Row)
create_row(RowSize, CurrentChar, Row) :-
    create_row_aux(RowSize, CurrentChar, [], Row).

create_row_aux(0, _, Row, Row).
create_row_aux(N, CurrentChar, Acc, Row) :-
    N > 0,
    N1 is N - 1,
    switch_char(CurrentChar, NextChar),
    create_row_aux(N1, NextChar, [CurrentChar | Acc], Row).

% create_board(+BoardSize, -Board)
create_board(BoardSize, Board) :-
    create_board_aux(BoardSize, 0, [], Board).

create_board_aux(N, Counter, Acc, List) :-
    Counter mod 2 =:= 0,
    create_row(N, red, Row),
    Counter < N - 1,
    create_board_aux(N, Counter + 1, [Row | Acc], List).

create_board_aux(N, Counter, Acc, [Row | Acc]) :-
    Counter mod 2 =:= 0,
    create_row(N, red, Row),
    Counter >= N - 1.

create_board_aux(N, Counter, Acc, List) :-
    Counter mod 2 =\= 0,
    create_row(N, blue, Row),
    Counter < N - 1,
    create_board_aux(N, Counter + 1, [Row | Acc], List).

create_board_aux(N, Counter, Acc, [Row | Acc]) :-
    Counter mod 2 =\= 0,
    create_row(N, blue, Row),
    Counter >= N - 1.

% create_all_board(+BoardSize, -Board)
create_all_board(BoardSize, Board) :-
    create_board(BoardSize, B),
    Last is BoardSize - 1,
    replace_piece(B, empty, 0-0, _B1),
    replace_piece(_B1, empty, 0-Last, _B2),
    replace_piece(_B2, empty, Last-0, _B3),
    replace_piece(_B3, empty, Last-Last, Board).

% game_loop(+GameState)
game_loop(Board-Player) :-
    print_board(Board),
    coordinates(SourceRow-SourceCol, DestRow-DestCol),
    move(Board-Player, SourceRow-SourceCol-DestRow-DestCol, NewGameState),
    game_loop(NewGameState).
