%% -*- mode : prolog -*-
:- module(intcode, [step_program/9,
		    step_program_no_base/7,
		    run_program_no_base/5,
		    get_program/2,
		    run_program/4]).

get_program(File, Prog) :-
    open(File, read, S), read_line_to_string(S, ProgStr),
    close(S), atomics_to_string(ProgAtoms, ",", ProgStr),
    maplist(atom_number, ProgAtoms, Prog).

mult(X, Y, Z) :- Z is X*Y.
eq(X, Y, Z) :- (X = Y -> Z = 1 ; Z = 0).
lt(X, Y, Z) :- (X < Y -> Z = 1 ; Z = 0).
padd(ListIn, Elem, N, ListOut) :-
    (N =< 0 -> ListIn = ListOut;
     plus(Nm, 1, N), padd(ListIn, Elem, Nm, T), ListOut = [Elem | T]).
fill_op(OpL, FilledOpL) :-
    length(OpL, L), plus(PaddL, L, 5),
    padd(OpL, 48, PaddL, FilledOpL).
explicit_modes(48, p). explicit_modes(49, i). explicit_modes(50, r).
parse_opcode(Code, Op) :-
    atom_codes(Code, OpL), fill_op(OpL, T),
    T = [M3, M2, M1, O2, O1], O2N is O2 - 48, O1N is O1 - 48, O is O2N*10 + O1N,
    maplist(explicit_modes, [M1, M2, M3], M),
    (O = 1, Op = (plus, M); O = 2, Op = (mult, M); O = 3, Op = (store, M);
     O = 4, Op = (return, M); O = 99, Op = (halt, M); O = 5, Op = (jit, M);
     O = 6, Op = (jif, M); O = 7, Op = (lt, M); O = 8, Op = (eq, M);
     O = 9, Op = (base, M)).

instr_len(I, L) :-
    [(plus, 4), (mult, 4), (store, 2), (return, 2), (halt, 1), (base, 2),
     (jit, 3), (jif, 3), (lt, 4), (eq, 4)] = IL,
    member((I, L), IL).
split(N, L1, L2, L3) :-
    length(L2, N), append(L2, L3, L1).
parse_program_([], Insts, Acc) :- reverse(Insts, Acc).
parse_program_([0 | _], Insts, Acc) :- reverse(Insts, Acc). % Stop if you hit memory
parse_program_(Codes, Insts, Acc) :-
    Codes = [Code | _], parse_opcode(Code, (Op, Ms)), instr_len(Op, L),
    split(L, Codes, [_ | Params], Rest),
    (Op = halt -> reverse(Insts, [(Op, Params, Ms) | Acc]);
     parse_program_(Rest, Insts, [(Op, Params, Ms) | Acc])).
    

parse_program(Codes, Insts) :-
    parse_program_(Codes, Insts, []).
    
pad_last(N, Elem, List1, List2) :- length(List1, M), plus(M, Extra, N),
				   pad_last_(Extra, Elem, List1, List2, []).
pad_last_(0, _, [], L2, Acc) :- reverse(L2, Acc).
pad_last_(N, E, [], L2, Acc) :- plus(Nm, 1, N), Nm >= 0, pad_last_(Nm, E, [], L2, [E | Acc]).
pad_last_(N, E, [L | Ls], L2, Acc) :- pad_last_(N, E, Ls, L2, [L | Acc]).

getElem(Idx, Val, InProg, OutProg) :-
    nth0(Idx, InProg, Val), InProg = OutProg; %% fails if Idx is larger than InProg length.
    length(InProg, L), L =< Idx, Val = 0, plus(Idx, 1, NewL), pad_last(NewL, 0, InProg, OutProg).
    
storeElem(Idx, Val, InProg, OutProg) :-
    nth0(Idx, InProg, _, X), nth0(Idx, OutProg, Val, X);
    length(InProg, L), L =< Idx, plus(Idx, 1, NewL), pad_last(NewL, 0, InProg, Padded),
    nth0(Idx, Padded, _, X), nth0(Idx, OutProg, Val, X).

get_arg(Pos, MEMIN, MEMOUT, Mode, Base, Arg) :-
    Mode = i, nth0(Pos, MEMIN, Arg), MEMIN = MEMOUT;
    Mode = p, nth0(Pos, MEMIN, Pointer), getElem(Pointer, Arg, MEMIN, MEMOUT);
    Mode = r, nth0(Pos, MEMIN, Pointer), plus(Pointer, Base, NewPointer),
    getElem(NewPointer, Arg, MEMIN, MEMOUT).

handle_arith((Op, [M1, M2, M3]), IP, NewIP, MEMIN, MEMOUT, Base) :-
    plus(IP, 4, NewIP), plus(IP, 1, Arg1P),
    plus(IP, 2, Arg2P), plus(IP, 3, Arg3P),
    get_arg(Arg1P, MEMIN, MEM1, M1, Base, Arg1),
    get_arg(Arg2P, MEM1, MEM2, M2, Base, Arg2),
    get_arg(Arg3P, MEM2, MEM3, i, Base, Arg3T),
    (M3 = r -> plus(Arg3T, Base, Arg3); Arg3 = Arg3T),
    call(Op, Arg1, Arg2, Out), storeElem(Arg3, Out, MEM3, MEMOUT).

handle_jumps((Op, [M1, M2, _]), IP, NewIP, MEMIN, MEMOUT, Base) :-
    plus(IP, 1, Arg1P), plus(IP, 2, Arg2P),
    get_arg(Arg1P, MEMIN, MEM1, M1, Base, Arg1),
    get_arg(Arg2P, MEM1, MEMOUT, M2, Base, Arg2),
    ((Arg1 = 0, Op = jif ; Arg1\= 0, Op = jit) -> Arg2 = NewIP; plus(IP, 3, NewIP)).

step_program(IP, MEMIN, MEMOUT, NewIP, Input, Output, BaseIn, BaseOut, Status) :-
    nth0(IP, MEMIN, OpRaw), parse_opcode(OpRaw, Op),
    (Op = (halt, _), MEMIN = MEMOUT, Status = s, NewIP = IP, BaseIn = BaseOut
    ;
    Op = (return, [M | _]), plus(IP, 2, NewIP), plus(IP, 1, ArgPos),
    get_arg(ArgPos, MEMIN, MEMOUT, M, BaseIn, Output), Status = o,
    BaseIn = BaseOut
    ;
    Op = (base, [M | _]), plus(IP, 2, NewIP), plus(IP, 1, ArgPos),
    get_arg(ArgPos, MEMIN, MEMOUT, M, BaseIn, Change),
    plus(BaseIn, Change, BaseOut), Status = c
    ; 
    Op = (store, [M | _]), plus(IP, 2, NewIP), plus(IP, 1, ArgPos), Status = i,
    get_arg(ArgPos, MEMIN, MT, i, BaseIn, IdxT),
    (M = r -> plus(BaseIn, IdxT, Idx); IdxT = Idx),
    storeElem(Idx, Input, MT, MEMOUT), BaseIn = BaseOut
    ;
    % handle_jumps(OP, IP, NewIP, MEMIN, MEMOUT), Status = c,
    Op = (O, _), (O = jit ; O = jif),  Status = c, BaseIn = BaseOut,
    handle_jumps(Op, IP, NewIP, MEMIN, MEMOUT, BaseIn)
    ;
    Op = (O, _), (O = plus ; O = mult ; O = lt ; O = eq), BaseIn = BaseOut,
    Status = c, handle_arith(Op, IP, NewIP, MEMIN, MEMOUT, BaseIn)).

step_program_no_base(IP, MEMIN, MEMOUT, NewIP, Input, Output, Status) :-
    step_program(IP, MEMIN, MEMOUT, NewIP, Input, Output, _, _, Status).

step_program_watch(IP, MEMIN, MEMOUT, NewIP, Input, Output,
	     BaseIn, BaseOut, Status, (WatchFrom, Data)) :-
    step_program(IP, MEMIN, MEMOUT, NewIP, Input, Output,
		 BaseIn, BaseOut, Status),
    (split(WatchFrom, MEMOUT, _, Data); Data = []).

run_program_no_base(IP, MEMIN, MEMOUT, Input, Outputs) :-
    step_program_no_base(IP, MEMIN, OP1, NIP1, Input, O1, S, _),
    (S = s -> MEMOUT = OP1, Outputs = []
    ;
    run_program_no_base(NIP1, OP1, MEMOUT, Input, RO, _),
    ((S = c; S = i) -> Outputs = RO ; Outputs = [O1 | RO])).

run_program((IP, MEMIN, Base), Input, (NewIP, MEMOUT, NewBase), Outputs) :-
    step_program(IP, MEMIN, MEMT, IPT, Input, OutT, Base, BaseT, S),
    (S = s -> MEMOUT = MEMT, Outputs = []
    ;
    run_program((IPT, MEMT, BaseT), Input, (NewIP, MEMOUT, NewBase), RestO),
    ((S = c; S = i) -> Outputs = RestO; Outputs = [OutT | RestO])).

run_program_nsteps(I, _, I, _, 0, _).
run_program_nsteps((IP, MEMIN, Base), Input,
		   (NewIP, MEMOUT, NewBase), Outputs, N, (WatchFrom, Data, BaseData)) :-
    N >= 0, plus(Nm, 1, N), 
    step_program_watch(IP, MEMIN, MEMT, IPT, Input, OutT, Base, BaseT, S, (WatchFrom, DataHead)),
    (S = s -> MEMOUT = MEMT, Outputs = [], Data = [DataHead], BaseData = [Base]
    ;
    run_program_nsteps((IPT, MEMT, BaseT), Input,
		       (NewIP, MEMOUT, NewBase), RestO, Nm, (WatchFrom, DataRest, BDRest)),
    Data = [DataHead | DataRest], BaseData = [BaseT | BDRest],
    ((S = c; S = i) -> Outputs = RestO; Outputs = [OutT | RestO])).
