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
print_row([]) :- nl.
print_row([H]) :-  % This rule will apply when there is only one element in the list
    translate(H, P),
    write('|'),
    write(P),
    write('|'),
    nl.
print_row([H | T]) :-
    translate(H, P),
    write('|'),
    write(P),
    print_row(T).

% Helper predicate to print a line of underscores
print_underscores(0).
print_underscores(Num) :-
    Num > 0,
    write(' ____ '),
    NewNum is Num - 1,
    print_underscores(NewNum).

% print_board(+Board) - handle the first row separately
print_board([]).
print_board([H | T]) :-
    length(H, Len),
    print_underscores(Len), 
    nl,
    print_row(H), 
    print_underscores(Len), 
    nl,
    print_rest_of_board(T, Len).

% print_rest_of_board(+Board) - handle the rest of the rows
print_rest_of_board([], _).
print_rest_of_board([H | T], Len) :-
    print_row(H), 
    print_underscores(Len), 
    nl,
    print_rest_of_board(T, Len).


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
translate(empty, '    ').
translate(blue, '  B  ').
translate(red, '  r  ').

