% menu.pl
% bounce
% *menu
% get*


:- consult(utils).


% bounce/0
bounce :-
    write('----------------------\n'),
    write('Welcome to Bounce Game\n'),
    write('----------------------\n').


% initial_menu(-Option)
initial_menu(Option) :- 
    write('Select the game mode\n'),
    write('1 - Human vs Human\n'),
    write('2 - Human vs Bot\n'),
    write('3 - Bot vs Bot\n'),
    read_option(1, 3, Option).


% bot_menu(-Option)
bot_menu(Option) :-
    write('Select the bot type\n'),
    write('1 - Easy Bot\n'),
    write('2 - Hard Bot\n'),
    read_option(1, 2, Option).


% board_menu(-BoardSize)
board_menu(BoardSize) :-
    write('Board size (should be an even number between 4 and 10): '),
    repeat,
    read_option(4, 10, BoardSize),
    BoardSize mod 2 =:= 0,
    !.


% get_color(+Player)
get_color(Player):-
    write('Select the color you would like to play\n'),
    write('1 - Red\n'),
    write('2 - Blue\n'),
    read_option(1, 2, Player).


% get_name(+Player)
get_name(Player) :-
    write('What is your name, player '),
    write(Player),
    write('?'),
    nl,
    read_string(Name),
    asserta(player_name(Player, Name)).
