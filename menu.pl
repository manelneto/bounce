:- consult(utils).

% bounce/0
bounce :-
    write('----------------------\n'),
    write('Welcome to Bounce Game\n'),
    write('----------------------\n').

% initial_menu/0
initial_menu :- 
    write('Select the game mode\n'),
    write('1 - Human vs Human\n'),
    write('2 - Human vs Bot\n'),
    write('3 - Bot vs Bot\n').

% bot_menu/0
bot_menu :-
    write('Select the bot type\n'),
    write('1 - Easy Bot\n'),
    write('2 - Hard Bot\n').

% board_menu(-BoardSize)
board_menu(BoardSize) :-
    write('Board size (should be even and greater than 3): \n'),
    repeat,
    read_number(BoardSize),
    BoardSize mod 2 =:= 0,
    BoardSize > 3,
    BoardSize < 11.

get_color(Player):-
    write('Select the color you would like to play\n'),
    write('1 - Red\n'),
    write('2 - Blue\n'),
    read_number(Player).

get_name(Player) :-
    write('What is your name, player '),
    write(Player),
    write('?'),
    nl,
    read_string(Name, []),
    asserta(player_name(Player, Name)).
    