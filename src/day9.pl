%% -*- mode: prolog -*-
:- use_module(intcode).

part9_1(File, Ans) :-
    get_program(File, Prog), run_program((0, Prog, 0), 1, _, Ans).
part9_2(File, Ans) :-
    get_program(File, Prog), run_program((0, Prog, 0), 2, _, Ans).
