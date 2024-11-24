:- use_module(library(socket)).
:- use_module(library(random)).

:- dynamic path/1.

path_list(Path):-Path = [north, south, south, east, west, north].

/* Convert to lower case if necessary,
skips some characters,
works with non latin characters in SWI Prolog. */
filter_codes([], []).
filter_codes([H|T1], T2) :-
    char_code(C, H),
    member(C, ['(', ')', ':', ',', '>']),
    filter_codes(T1, T2).
filter_codes([H|T1], [F|T2]) :-
    code_type(F, to_lower(H)),
    filter_codes(T1, T2).


process(Stream) :-
    format(atom(CommandBark), 'say bark bark!~n', []),
    write('CommandBark: '),write(CommandBark),
    write(Stream, CommandBark),

    path([Exit|Rest]),
    format(atom(Command), 'move ~w~n', [Exit]),
    write('Command: '),write(Command),
    write(Stream, Command),
    flush_output(Stream),
    (Rest == [] -> 
        retract(path(_)), path_list(Path), assertz(path(Path));
        retract(path(_)), assertz(path(Rest))).
process(_).
    

create_name(Stream):-
    format(atom(Command), 'Milka, the dog~n',[]),
    write(Stream,Command),
    write(Command),
    flush_output(Stream).

read_stream(Stream,Tokens):-
    read_lines_to_codes(Stream, Codes),
    filter_codes(Codes, Filtered),
    atom_codes(Atom, Filtered),
    tokenize_atom(Atom, Tokens),
    write(Stream),
    nl,
    flush_output().

command_in_room(Stream):-
    random(1,15,R),
    sleep(R),
    process(Stream).

loop(Stream) :-
    read_stream(Stream, Tokens),
    command_in_room(Stream),
    loop(Stream).

main :-
    setup_call_cleanup(
        tcp_connect(localhost:3333, Stream, []),
        (create_name(Stream), loop(Stream)),
        close(Stream)).
     
