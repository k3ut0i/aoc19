%% -*- mode : prolog -*-
mult(X, Y, Z) :- Z is X*Y.
eq(X, Y, Z) :- (X = Y -> Z = 1 ; Z = 0).
lt(X, Y, Z) :- (X < Y -> Z = 1 ; Z = 0).
padd(ListIn, Elem, N, ListOut) :-
    (N =< 0 -> ListIn = ListOut;
     plus(Nm, 1, N), padd(ListIn, Elem, Nm, T), ListOut = [Elem | T]).
fill_op(OpL, FilledOpL) :-
    length(OpL, L), plus(PaddL, L, 5),
    padd(OpL, 48, PaddL, FilledOpL).
explicit_modes(48, p). explicit_modes(49, i).
parse_opcode(Code, Op) :-
    atom_codes(Code, OpL), fill_op(OpL, T),
    T = [M3, M2, M1, O2, O1], O2N is O2 - 48, O1N is O1 - 48, O is O2N*10 + O1N,
    maplist(explicit_modes, [M1, M2, M3], M),
    (O = 1, Op = (plus, M); O = 2, Op = (mult, M); O = 3, Op = (store, M);
     O = 4, Op = (return, M); O = 99, Op = (halt); O = 5, Op = (jit, M);
     O = 6, Op = (jif, M); O = 7, Op = (lt, M); O = 8, Op = (eq, M)).

instr_len(I, L) :-
    [(plus, 4), (mult, 4), (store, 2), (return, 2), (halt, 1),
     (jump_if_true, 3), (jump_if_false, 3), (less_than, 4), (equals, 4)] = IL,
    member((I, L), IL).
storeElem(Idx, Val, InProg, OutProg) :- nth0(Idx, InProg, _, X),
					nth0(Idx, OutProg, Val, X).
handle_arith(Op, [M1, M2, _], [I1, I2, I3], InProg, OutProg) :-
    (M1 = p -> nth0(I1, InProg, Arg1); Arg1 = I1),
    (M2 = p -> nth0(I2, InProg, Arg2); Arg2 = I2),
    call(Op, Arg1, Arg2, Out), storeElem(I3, Out, InProg, OutProg).

handle_jumps(Op, [M1, M2, _], [I1, I2], InProg, IP, NewIP) :-
    (M1 = p -> nth0(I1, InProg, Arg1) ; Arg1 = I1),
    (M2 = p -> nth0(I2, InProg, Arg2) ; Arg2 = I2),
    (Arg1 = 0, Op = jif ; Arg1\= 0, Op = jit) ->
	Arg2 = NewIP; plus(IP, 3, NewIP).

step_program(IP, InProg, OutProg, NewIP, Input, Output, Status) :-
    nth0(IP, InProg, OpRaw), parse_opcode(OpRaw, Op),
    (Op = (halt), InProg = OutProg, Status = s
    ;
    Op = (return, [M | _]), plus(IP, 2, NewIP), plus(IP, 1, ArgPos),
    InProg = OutProg, nth0(ArgPos, InProg, Arg),
    (M = p -> nth0(Arg, InProg, Output) ; Output = Arg), Status = o
    ;
    Op = (store, _), plus(IP, 2, NewIP), plus(IP, 1, ArgPos), Status = c,
    nth0(ArgPos, InProg, Idx), storeElem(Idx, Input, InProg, OutProg)
    ;
    Op = (O, M), (O = jit ; O = jif), plus(IP, 1, Arg1P), plus(IP, 2, Arg2P),
    nth0(Arg1P, InProg, Arg1), nth0(Arg2P, InProg, Arg2), Status = c,
    handle_jumps(O, M, [Arg1, Arg2], InProg, IP, NewIP), InProg = OutProg
    ;
    Op = (O, M), (O = plus ; O = mult ; O = lt ; O = eq),
    plus(IP, 4, NewIP), plus(IP, 1, Arg1P),
    plus(IP, 2, Arg2P), plus(IP, 3, Arg3P), nth0(Arg1P, InProg, Arg1),
    nth0(Arg2P, InProg, Arg2),  nth0(Arg3P, InProg, Arg3), Status = c,
    handle_arith(O, M, [Arg1, Arg2, Arg3], InProg, OutProg)).

run_program(InProg, OutProg, Input, Outputs) :-
    run_program_(0, InProg, OutProg, Input, Outputs).
run_program_(IP, InProg, OutProg, Input, Outputs) :-
    step_program(IP, InProg, OP1, NIP1, Input, O1, S),
    (S = s -> OutProg = OP1, Outputs = []
    ;
    run_program_(NIP1, OP1, OutProg, Input, RO),
    (S = c -> Outputs = RO ; Outputs = [O1 | RO])).
test_diagnostic(File, ID, Diag_Code) :-
    open(File, read, Stream),
    read_line_to_string(Stream, ProgStr),
    close(Stream),
    atomics_to_string(IStr, ",", ProgStr),
    maplist(atom_number, IStr, InitState),
    run_program(InitState, _, ID, Diag_Code).
