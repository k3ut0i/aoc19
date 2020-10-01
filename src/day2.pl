%% -*- mode: prolog -*-

%% run_program(Init, Final).
% look_up(Idx, Val, Prog). %nth0 covers this.
store(Idx, Val, InProg, OutProg) :- nth0(Idx, InProg, _, X),
				    nth0(Idx, OutProg, Val, X).
multiply(X, Y, Z) :- Z is X*Y.
plus1(X, Y) :- plus(X, 1, Y).
execute_op(F, In1, In2, Out, InProg, OutProg) :- nth0(In1, InProg, X),
						 nth0(In2, InProg, Y),
						 call(F, X, Y, Z),
						 store(Out, Z, InProg, OutProg).
%% run a single instruction of the InProg starting at IP.
step_program(IP, InProg, OutProg, S) :-
    nth0(IP, InProg, 99), S = s, InProg = OutProg;
    (plus1(IP ,In1P), plus1(In1P, In2P), plus1(In2P, OutP),
     nth0(In1P, InProg, In1), nth0(In2P, InProg, In2), nth0(OutP, InProg, Out),
     (nth0(IP, InProg, 1), execute_op(plus, In1, In2, Out, InProg, OutProg), S=c;
      nth0(IP, InProg, 2), execute_op(multiply, In1, In2, Out, InProg, OutProg), S=c)).


run_program(InProg, OutProg) :- run_program(InProg, OutProg, 0).
run_program(InProg, OutProg, IP) :-
    step_program(IP, InProg, T, S), NIP is IP + 4,
    (S =  s -> T = OutProg; run_program(T, OutProg, NIP)).

%%% Some test examples which helped debug.
test :- run_program([1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50],
		    [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]),
	run_program([1, 0, 0, 0, 99], [2, 0, 0, 0, 99]),
	run_program([2, 3, 0, 3, 99], [2, 3, 0, 6, 99]),
	run_program([2, 4, 4, 5, 99, 0], [2, 4, 4, 5, 99, 9801]),
	run_program([1, 1, 1, 4, 99, 5, 6, 0, 99], [30, 1, 1, 4, 2, 5, 6, 0, 99]).

restore1202(In, Out) :-setinput(12, 2, In, Out).

setinput(Noun, Verb, InProg, OutProg) :-
    store(1, Noun, InProg, T), store(2, Verb, T, OutProg).

output(File, Noun, Verb, Ans) :-
    open(File, read, Stream, []),
    read_line_to_string(Stream, ProgStr),
    close(Stream),
    atomics_to_string(IStr, ",", ProgStr),
    maplist(atom_number, IStr, InitState),
    setinput(Noun, Verb, InitState, CorrectState),
    run_program(CorrectState, FinalState),
    nth0(0, FinalState, Ans).

find_inputs(File, Output, Noun, Verb) :-
    between(0, 99, Noun), between(0, 99, Verb), output(File, Noun, Verb, Output).
