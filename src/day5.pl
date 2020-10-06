%% -*- mode : prolog -*-
:- use_module(intcode).

run_program(InProg, OutProg, Input, Outputs) :-
    run_program_no_base(0, InProg, OutProg, Input, Outputs).

test_diagnostic(File, ID, Diag_Code) :-
    get_program(File, InitState),
    run_program(InitState, _, ID, Diag_Code).
