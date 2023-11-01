% read_number(-X)
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

% print_board_line(+BoardSize)
print_board_line(0) :-
    write(' '),
    nl.
print_board_line(BoardSize) :-
    write(' ---'),
    BoardSize1 is BoardSize - 1,
    print_board_line(BoardSize1).

% print_grid_line(+N, +BoardSize)
print_grid_line(BoardSize, BoardSize) :-
    nl.
print_grid_line(N, BoardSize) :-
    write('  '),
    write(N),
    write(' '),
    N1 is N + 1,
    print_grid_line(N1, BoardSize).

% print_row(+Row)
print_row([]) :-
    nl.
print_row([H | T]) :-
    translate(H, P),
    write(' '),
    write(P),
    write(' |'),
    print_row(T).

% print_board(+Board)
print_board([H], N) :-
    length(H, BoardSize),
    write('    '),
    print_board_line(BoardSize),
    write('  '),
    write(N),
    write(' |'),
    print_row(H),
    write('    '),
    print_board_line(BoardSize).
print_board([H | T], N) :-
    length(H, BoardSize),
    write('    '),
    print_board_line(BoardSize),
    write('  '),
    write(N),
    write(' |'),
    print_row(H),
    N1 is N + 1,
    print_board(T, N1).

read_string(NamePlayer, List):-
    get_char(Char),
    Char \= '\n',
    append(List, [Char], ListR),
    read_string(NamePlayer, ListR).
read_string(NamePlayer, List):-
    atom_chars(NamePlayer, List).

% coordinates(-Position)
coordinates(Row-Col) :-
    write('You do not have a valid move. Remove your piece from\n'),
    write('Row number: \n'),
    read_number(Row),
    write('Column number: \n'),
    read_number(Col).

% coordinates(-SourcePosition, -DestPosition)
coordinates(SourceRow-SourceCol, DestRow-DestCol) :-
    write('Move your piece from\n'),
    write('Row number: \n'),
    read_number(SourceRow),
    write('Column number: \n'),
    read_number(SourceCol),
    write('To: \n'),
    write('Row number: \n'),
    read_number(DestRow),
    write('Column number: \n'),
    read_number(DestCol).

% translate(?Internal, ?External)
translate(empty, ' ').
translate(blue, 'B').
translate(red, 'r').

