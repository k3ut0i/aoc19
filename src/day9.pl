%% -*- mode: prolog -*-
:- use_module(intcode).

part9_1(File, Ans) :-
    get_program(File, Prog), run_program((0, Prog, 0), 1, _, Ans).

part9_2(File, Ans) :-
    get_program(File, Prog), run_program((0, Prog, 0), 2, _, Ans).

write_list_of_lists_([], Stream) :- nl(Stream).
write_list_of_lists_([L | LL], Stream) :-
    write_list(L, Stream), write_list_of_lists_(LL, Stream).
write_list([], Stream) :- nl(Stream).
write_list([X | Xs], Stream) :-
    format(Stream, "~w ", [X]), write_list(Xs, Stream).
    
write_list_of_lists_to_file(LL, File) :-
    open(File, write, Stream),
    write_list_of_lists_(LL, Stream),
    close(Stream).
write_list_to_file(L, File) :-
    open(File, write, Stream),
    write_list(L, Stream),
    close(Stream).
