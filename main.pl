:- consult(menu).

% display_game(+GameState).
display_game(GameState) :-
    write(GameState).

play(RowSize, ElemSize, Board) :-
    create_board(RowSize, [], Board, ElemSize),
    display_game(Board).

create_board(0, Board, Board, _) :- !.

create_board(RowSize, TempBoard, Board, ElemSize) :-
    RowSize > 0,
    length(Newlist, ElemSize),
    append(TempBoard, [Newlist], NewBoard),
    RowSize1 is RowSize - 1,
    create_board(RowSize1, NewBoard, Board, ElemSize).


first line(Bs, [empty, blue, red, a])