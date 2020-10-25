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

brute_peroid(N, S) :-
    step(S, S1), brute_period(N, S1, S, 1).
brute_period(N, S, S, N).
brute_period(N, S, A, C) :-
    step(S, ST), (ST = A, N is C+1;
		  C1 is C+1, brute_period(N, ST, A, C1)).
    
