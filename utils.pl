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

% print_row(+Row)
print_row([]) :- nl, nl.
print_row([H | T]) :-
    translate(H, P),
    write(P),
    write('  '),
    print_row(T).

% print_board(+Board)
print_board([]).
print_board([H | T]) :-
    print_row(H),
    print_board(T).

% coordinates(-SourcePosition, -DestPosition)
coordinates(SourceRow-SourceCol, DestRow-DestCol) :-
    write('Move your piece from\n'),
    write('Row number: \n'),
    repeat,
    read_number(SourceRow),
    write('Column number: \n'),
    repeat,
    read_number(SourceCol),
    write('To: \n'),
    write('Row number: \n'),
    repeat,
    read_number(DestRow),
    write('Column number: \n'),
    repeat,
    read_number(DestCol).

% translate(?Internal, ?External)
translate(empty, '.').
translate(blue, 'B').
translate(red, 'r').
