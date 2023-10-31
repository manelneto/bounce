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

read_string(NamePlayer, List):-
    get_char(Char),
    Char \= '\n',
    append(List, [Char], ListR),
    get_line(NamePlayer, ListR).
read_string(NamePlayer, List):-
    atom_chars(NamePlayer, List).


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
translate(empty, ' ').
translate(blue, 'B').
translate(red, 'r').

