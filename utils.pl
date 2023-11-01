% utils.pl
% read_*
% print_*
% translate

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


% read_string(String, List)
read_string(NamePlayer, List):-
    get_char(Char),
    Char \= '\n',
    append(List, [Char], ListR),
    read_string(NamePlayer, ListR).

read_string(NamePlayer, List):-
    atom_chars(NamePlayer, List).


% read_coordinates(-Position)
read_coordinates(Row-Col) :-
    write('You do not have a valid move. Remove your piece from...\n'),
    write('Row: '),
    read_number(Row),
    write('Column: '),
    read_number(Col).


% read_coordinates(-SourcePosition, -DestPosition)
read_coordinates(SourceRow-SourceCol, DestRow-DestCol) :-
    write('Move your piece from...\n'),
    write('Row: '),
    read_number(SourceRow),
    write('Column: '),
    read_number(SourceCol),
    write('Move your piece to...\n'),
    write('Row: '),
    read_number(DestRow),
    write('Column: '),
    read_number(DestCol).


% print_header(+N, +HeaderSize)
print_header(N, HeaderSize) :-
    write('   '),
    print_header_aux(N, HeaderSize).

print_header_aux(HeaderSize, HeaderSize) :-
    nl.

print_header_aux(N, HeaderSize) :-
    N < HeaderSize,
    write('  '),
    write(N),
    write(' '),
    N1 is N + 1,
    print_header_aux(N1, HeaderSize).


% print_board_line(+LineSize)
print_board_line(LineSize) :-
    write('   '),
    print_board_line_aux(LineSize).

print_board_line_aux(0) :-
    nl.

print_board_line_aux(LineSize) :-
    LineSize > 0,
    write(' ---'),
    LineSize1 is LineSize - 1,
    print_board_line_aux(LineSize1).


% print_row(+Row)
print_row([]) :-
    nl.

print_row([H | T]) :-
    translate(H, P),
    write(' '),
    write(P),
    write(' |'),
    print_row(T).


% print_board(+Board, +N)
print_board([], _).
print_board([H | T], N) :-
    length(H, LineSize),
    print_board_line(LineSize),
    print_row([N | H]),
    N1 is N + 1,
    print_board(T, N1).


% print_turn(+Name)
print_turn(Name) :-
    write('It is your turn, '),
    write(Name),
    write('!'),
    nl.


% translate(?InternalRepresentation, ?ExternalRepresentation)
translate(empty, ' ').
translate(blue, 'B').
translate(red, 'r').
translate(N, N) :- number(N).
