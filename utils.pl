% utils.pl
% read*
% print*
% invert
% flatten
% replace
% translate


:- use_module(library(between)).
:- use_module(library(lists)).


% read_number(-X)
% read an input number
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
% read an input string
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
% validate input options
read_option(Min, Max, Option) :-
    repeat,
    read_number(Option),
    between(Min, Max, Option),
    !.


% read_coordinates(-Position)
% read input coordinates 
read_coordinates(Row-Col) :-
    write('You do not have a valid move. Remove your piece from...\n'),
    write('Row: '),
    read_number(Row),
    write('Column: '),
    read_number(Col).


% read_coordinates(-SourcePosition, -DestPosition)
% read input coordinates (Source and Destination)
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
% print the header of the game
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
% print a board line with an specific size
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
% print a board row
print_row([]) :-
    nl.

print_row([H | T]) :-
    translate(H, P),
    write(' '),
    write(P),
    write(' |'),
    print_row(T).


% print_board(+Board, +N)
% print the board of the game
print_board([], _).
print_board([H | T], N) :-
    length(H, LineSize),
    print_board_line(LineSize),
    print_row([N | H]),
    N1 is N + 1,
    print_board(T, N1).


% print_turn(+Name)
% print the currently turn
print_turn(Name) :-
    nl,
    write('It is your turn, '),
    write(Name),
    write('!\n'),
    nl.


% invert(+List, -Inverted)
% invert the members of a list
invert(List, Inverted) :-
    invert_aux(List, [], Inverted).

invert_aux([], Inverted, Inverted).

invert_aux([H | T], Acc, Inverted) :-
    invert_aux(T, [H | Acc], Inverted).


% flatten(+NestedList, -FlatList)
% flatten list
flatten(NestedList, FlatList) :-
    flatten_aux(NestedList, [], FlatListInverted),
    invert(FlatListInverted, FlatList).

flatten_aux([], FlatList, FlatList).

flatten_aux([H | T], Acc, FlatList) :-
    invert(H, HInverted),
    append(HInverted, Acc, Acc1),
    flatten_aux(T, Acc1, FlatList).    


% replace(+List, +Element, +Index, -NewList)
% replace an Element with Index of a List to a NewList
replace(List, Element, Index, NewList) :-
    nth0(Index, List, _, _L),
    nth0(Index, NewList, Element, _L).


% translate(?InternalRepresentation, ?ExternalRepresentation)
% represents the translation from internal representation to external representation
translate(empty, ' ').
translate(blue, 'B').
translate(red, 'r').
translate(N, N) :- number(N).
