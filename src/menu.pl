% menu.pl


% bounce/0
% represents the beginning of the game 
bounce :-
    write('----------------------\n'),
    write('Welcome to Bounce Game\n'),
    write('----------------------\n').


% initial_menu(-Option)
% menu to select the game mode
initial_menu(Option) :- 
    write('Select the game mode\n'),
    write('1 - Human vs Human\n'),
    write('2 - Human vs Bot\n'),
    write('3 - Bot vs Bot\n'),
    read_option(1, 3, Option),
    nl.


% bot_menu(-Option)
% menu to select the bot type
bot_menu(Option) :-
    write('Select the bot type\n'),
    write('1 - Easy Bot\n'),
    write('2 - Greedy Bot\n'),
    write('3 - Hard Bot\n'),
    read_option(1, 3, Option),
    nl.


% board_menu(-BoardSize)
% menu to get the size of the board
board_menu(BoardSize) :-
    write('Board size (should be an even number between 4 and 10):\n'),
    repeat,
    read_option(4, 10, BoardSize),
    BoardSize mod 2 =:= 0,
    !,
    nl.


% get_color(+Name, -Color)
% menu to get the color of a player
get_color(Name, Color):-
    write('Select the color for '),
    write(Name),
    nl,
    write('1 - Red\n'),
    write('2 - Blue\n'),
    read_option(1, 2, Color),
    nl.


% get_name(-Name)
% menu to get the name of a human player
get_name(Name) :-
    write('What is your name?\n'),
    read_string(Name),
    nl.
