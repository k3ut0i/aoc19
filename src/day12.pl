%% -*- mode: prolog -*-

init_state(S) :-
    list_to_assoc([io-([17, 5, 1], [0, 0, 0]),
		   europa-([-2, -8, 8], [0, 0, 0]),
		   ganymede-([7, -6, 14], [0, 0, 0]),
		   callisto-([1, -10, 4], [0, 0, 0])],
		  S).
test_state(S) :-
    list_to_assoc([io-([-1, 0, 2], [0, 0, 0]),
		   europa-([2, -10, -7], [0, 0, 0]),
		   ganymede-([4, -8, 8], [0, 0, 0]),
		   callisto-([3, 5, -1], [0, 0, 0])],
		  S).
test_state1(S) :-
    list_to_assoc([io-([-8, -10, 0], [0, 0, 0]),
		   europa-([5, 5, 10], [0, 0, 0]),
		   ganymede-([2, -7, 3], [0, 0, 0]),
		   callisto-([9, -8, -3], [0, 0, 0])],
		  S).

velocity_change_on_axis(X, X, 0).
velocity_change_on_axis(X1, X2, 1) :- X1 > X2.
velocity_change_on_axis(X1, X2, -1) :- X2 > X1.
    
% apply gravity of 1 moon on 2 moon.
apply_gravity_2((P1, _), (P2, V2), (P2, V2N)) :-
    maplist(velocity_change_on_axis, P1, P2, C),
    maplist(plus, V2, C, V2N).

% apply gravity of M moon on the rest
apply_gravity_1(M, S1, S2) :-
    gen_assoc(M, S1, MS), map_assoc(apply_gravity_2(MS), S1, S2).

apply_gravity(S1, S2) :-
    foldl([M]>>apply_gravity_1(M), [io, europa, ganymede, callisto], S1, S2).

apply_velocity_1((P1, V1), (P2, V1)) :-
    maplist(plus, V1, P1, P2).

apply_velocity(S1, S2) :-
    map_assoc(apply_velocity_1, S1, S2).

step(S1, S2) :-
    apply_gravity(S1, ST), apply_velocity(ST, S2).

simulate_n(0, S, S).
simulate_n(N, S1, S2) :-
    N > 0, plus(N1, 1, N), step(S1, ST), simulate_n(N1, ST, S2).

abs_plus(O1, O2, S) :-
    P1 is abs(O1), P2 is abs(O2), plus(P1, P2, S).
energy_of((P, V), E) :- 
    foldl(abs_plus, P, 0, PE),
    foldl(abs_plus, V, 0, KE),
    E is PE*KE.
energy(S, E) :-
    map_assoc(energy_of, S, SE),
    assoc_to_values(SE, Es),
    foldl(plus, Es, 0, E).
part1(E) :-
    init_state(S), simulate_n(1000, S, S1), energy(S1, E).

%% Brute force calculation that blows the stack
%% Why is this blowing the stack? shouldn't tail call opt take care of it?
brute_peroid(N, S) :-
    step(S, S1), brute_period(N, S1, S, 1).
brute_period(N, S, S, N).
brute_period(N, S, A, C) :-
    step(S, ST), (ST = A, N is C+1;
		  C1 is C+1, brute_period(N, ST, A, C1)).
    
%% Finally figured it out. All the axes are decoupled, i.e., they are
%% independent of each other. In reality they should not be as the
%% acceleration is dependent on relative distance, but here it is just
%% constant amplitude.
%% So if Each axis has a period then the period of the system is the
%% lcm of all three periods.

axis_apply_gravity_1(Ps, X, V) :-
    maplist([P]>>velocity_change_on_axis(P, X), Ps, Vs),
    foldl(plus, Vs, 0, V).
axis_apply_gravity((Ps, Vs1), (Ps, Vs2)) :-
    maplist(axis_apply_gravity_1(Ps), Ps, DeltaV),
    maplist(plus, Vs1, DeltaV, Vs2).
axis_apply_velocity((Ps1, Vs), (Ps2, Vs)) :-
    maplist(plus, Ps1, Vs, Ps2).

axis_step(S1, S2) :-
    axis_apply_gravity(S1, ST),
    axis_apply_velocity(ST, S2).

axis_period(N, S) :-
    axis_step(S, S1), axis_period_(N, S1, S, 1).
axis_period_(N, S, S, N).
axis_period_(N, S, S1, C) :-
    axis_step(S, ST), C1 is C+1,
    (ST = S1, N = C1; axis_period_(N, ST, S1, C1)).

fst((X, _), X).
snd((_, X), X).
get_n(N, Lss, Ls) :- maplist(nth0(N), Lss, Ls).
decouple(S, (XP, XV), (YP, YV), (ZP, ZV)) :-
    assoc_to_values(S, Ss),
    maplist(fst, Ss, Ps),
    maplist(snd, Ss, Vs),
    get_n(0, Ps, XP), get_n(0, Vs, XV),
    get_n(1, Ps, YP), get_n(1, Vs, YV),
    get_n(2, Ps, ZP), get_n(2, Vs, ZV).
    
period(N, S) :-
    decouple(S, X, Y, Z),
    axis_period(NX, X),
    axis_period(NY, Y),
    axis_period(NZ, Z),
    N1 is lcm(NX, NY),
    N is lcm(NZ, N1).
