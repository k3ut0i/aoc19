%% -*- mode : prolog -*-
:- consult("day5.pl").

get_program(File, Prog) :-
    open(File, read, S), read_line_to_string(S, ProgStr),
    close(S), atomics_to_string(ProgAtoms, ",", ProgStr),
    maplist(atom_number, ProgAtoms, Prog).

set_phase(P, InProg, OutProg, NewIP) :-
    step_program(0, InProg, OutProg, NewIP, P, _, _).

run_amplifier(I, O, [P1, P2, P3, P4, P5], InitProg) :-
    set_phase(P1, InitProg, OP1, IP1),
    set_phase(P2, InitProg, OP2, IP2),
    set_phase(P3, InitProg, OP3, IP3),
    set_phase(P4, InitProg, OP4, IP4),
    set_phase(P5, InitProg, OP5, IP5),
    run_program_(IP1, OP1, _, I, [O1]),
    run_program_(IP2, OP2, _, O1, [O2]),
    run_program_(IP3, OP3, _, O2, [O3]),
    run_program_(IP4, OP4, _, O3, [O4]),
    run_program_(IP5, OP5, _, O4, [O]).


phases(P1, P2, P3, P4, P5) :-
    select(P1, [0, 1, 2, 3, 4], R1), select(P2, R1, R2),
    select(P3, R2, R3), select(P4, R3, [P5]).
highest_signal(Prog, S) :-
    findall(O, (phases(P1, P2, P3, P4, P5),
		run_amplifier(0, O, [P1, P2, P3, P4, P5], Prog)), Os),
    max_list(Os, S).
    
part7_1(File, Ans) :-
    get_program(File, Prog), highest_signal(Prog, Ans).

run_prog_in_sync(IP, NewIP, InProg, OutProg, Input, Output, Status) :-
    step_program(IP, InProg, OP1, NIP1, Input, Output, S),
    (S = s, OutProg = OP1, Status = halted, NewIP = NIP1
    ; %Run it until we encounter an output, halted or consumed the input.
    S = c, run_prog_in_sync(NIP1, NewIP, OP1, OutProg, Input, Output, Status)
    ;
    S = o, OutProg = OP1, NewIP = NIP1, Status = output % delivered output
    ;
    S = i, OutProg = OP1, NewIP = NIP1, Status = input). % consumed input
run_prog_one_cycle(IP, NewIP, Prog, NewProg, Input, Output, Status) :-
    (run_prog_in_sync(IP, IPO, Prog, ProgO, Input, _, input), Status = output,
     run_prog_in_sync(IPO, NewIP, ProgO, NewProg, _, Output, output))
    ;
    run_prog_in_sync(IP, NewIP, Prog, NewProg, Input, Output, halted).

run_all_amplifiers([IP1, IP2, IP3, IP4, IP5], [OP1, OP2, OP3, OP4, OP5],
		   Input, Output) :-
    run_prog_one_cycle(IP1, NIP1, OP1, NOP1, Input, O1, S),
    run_prog_one_cycle(IP2, NIP2, OP2, NOP2, O1, O2, S),
    run_prog_one_cycle(IP3, NIP3, OP3, NOP3, O2, O3, S),
    run_prog_one_cycle(IP4, NIP4, OP4, NOP4, O3, O4, S),
    run_prog_one_cycle(IP5, NIP5, OP5, NOP5, O4, OT, S),
    ((S = halted -> Input = Output);
     S = output -> run_all_amplifiers([NIP1, NIP2, NIP3, NIP4, NIP5],
				      [NOP1, NOP2, NOP3, NOP4, NOP5],
				      OT, Output)).
    
run_amplifier_feedback([P1, P2, P3, P4, P5], InProg, Input, Output) :-
    set_phase(P1, InProg, OP1, IP1),
    set_phase(P2, InProg, OP2, IP2),
    set_phase(P3, InProg, OP3, IP3),
    set_phase(P4, InProg, OP4, IP4),
    set_phase(P5, InProg, OP5, IP5),
    run_all_amplifiers([IP1, IP2, IP3, IP4, IP5],
		       [OP1, OP2, OP3, OP4, OP5],
		       Input, Output).
newphases(P1, P2, P3, P4, P5) :-
    select(P1, [5, 6, 7, 8, 9], R1), select(P2, R1, R2),
    select(P3, R2, R3), select(P4, R3, [P5]).
amplifier_highest_signal(Prog, S) :-
    findall(O, (newphases(P1, P2, P3, P4, P5),
		run_amplifier_feedback([P1, P2, P3, P4, P5], Prog, 0, O)), Os),
    max_list(Os, S).
    
part7_2(File, Ans) :-
    get_program(File, Prog), amplifier_highest_signal(Prog, Ans).
