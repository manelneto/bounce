% utils.pl


% read_number(-X)
% reads an input number
read_number(X) :-
    read_number_aux(0, false, X).

read_number_aux(Acc, _, X) :-
    get_code(C),
    between(48, 57, C),
    !,
    Acc1 is 10 * Acc + (C - 48),
    read_number_aux(Acc1, true, X).

read_number_aux(X, true, X).


% read_string(-String)
% reads an input string
read_string(String) :-
    read_string_aux([], String).

read_string_aux(CharList, String) :-
    get_char(Char),
    Char \= '\n',
    !,
    append(CharList, [Char], NewCharList),
    read_string_aux(NewCharList, String).

read_string_aux(CharList, String) :-
    atom_chars(String, CharList).


% read_option(+Min, +Max, -Option)
% reads and validates an input option
read_option(Min, Max, Option) :-
    repeat,
    read_number(Option),
    between(Min, Max, Option),
    !.


% read_coordinates(-Position)
% reads input coordinates 
read_coordinates(Row-Col) :-
    write('You do not have a valid move. Remove your piece from...\n'),
    write('Row: '),
    read_number(Row),
    write('Column: '),
    read_number(Col),
    nl.


% read_coordinates(-SourcePosition, -DestPosition)
% reads input coordinates (Source and Destination)
read_coordinates(SourceRow-SourceCol, DestRow-DestCol) :-
    write('Move your piece from...\n'),
    write('Row: '),
    read_number(SourceRow),
    write('Column: '),
    read_number(SourceCol),
    write('\nMove your piece to...\n'),
    write('Row: '),
    read_number(DestRow),
    write('Column: '),
    read_number(DestCol),
    nl.


% press_enter/0
% waits for the user to press ENTER
press_enter :-
    write('Press ENTER to continue\n'),
    repeat,
    get_char(C),
    C = '\n',
    !. 


% print_header(+N, +HeaderSize)
% prints the header of the board
print_header(N, HeaderSize) :-
    nl,
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
% prints a board line with a specific size
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
% prints a board row
print_row([]) :-
    nl.

print_row([H | T]) :-
    translate(H, P),
    write(' '),
    write(P),
    write(' |'),
    print_row(T).


% print_board(+Board, +N)
% prints the board of the game
print_board([], _).

print_board([H | T], N) :-
    length(H, LineSize),
    print_board_line(LineSize),
    print_row([N | H]),
    N1 is N + 1,
    print_board(T, N1).


% print_position(+Position)
% prints a position
print_position(Row-Col) :-
    write('('),
    write(Row),
    write(', '),
    write(Col),
    write(')').


% print_move(+Move)
% prints a move
print_move(SourceRow-SourceCol-DestRow-DestCol) :-
    write('Moved piece: '),
    print_position(SourceRow-SourceCol),    
    print(' --> '),
    print_position(DestRow-DestCol),
    nl.


% print_piece(+Position)
% prints the removal of a piece
print_piece(Position) :-
    print('Removed piece: '),
    print_position(Position),    
    nl.


% print_turn(+Name)
% prints the current turn
print_turn(Name) :-
    nl,
    write('It is your turn, '),
    write(Name),
    write('!\n'),
    nl.


% invert(+List, -Inverted)
% inverts a list
invert(List, Inverted) :-
    invert_aux(List, [], Inverted).

invert_aux([], Inverted, Inverted).

invert_aux([H | T], Acc, Inverted) :-
    invert_aux(T, [H | Acc], Inverted).


% max_value(+List, -Max)
% finds the the maximum value of a list
max_value([Max], Max).

max_value([H | T], H):-
    max_value(T, Max),
    H > Max.

max_value([H | T], Max):-
    max_value(T, Max),
    H =< Max.


% max_value_index(+List, -Index)
% finds the index of the maximum value of a list
max_value_index(List, Index) :-
    max_value(List, Max),
    nth0(Index, List, Max).


% replace(+List, +Element, +Index, -NewList)
% replaces the Index position of List with Element
replace(List, Element, Index, NewList) :-
    nth0(Index, List, _, _L),
    nth0(Index, NewList, Element, _L).


% translate(?InternalRepresentation, ?ExternalRepresentation)
% translates the internal representation of an element to its external representation
translate(empty, ' ').
translate(blue, 'B').
translate(red, 'r').
translate(N, N) :- number(N).
