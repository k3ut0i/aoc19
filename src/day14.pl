%% -*- mode: prolog -*-
:- use_module(fileutils).

parse_compound(Str, (C, N)) :-
    split_string(Str, " ", "", [N1, C1]),
    atom_string(C, C1),
    number_string(N, N1).

parse_reaction(Str, R) :-
    split_string(Str, "=>", " ", [I, "", O]),
    split_string(I, ",", " ", Is),
    maplist(parse_compound, Is, I1),
    parse_compound(O, (C, N)), R = C-(N, I1).

read_reactions(File, Rs) :-
    read_all_lines(File, Lines),
    maplist(parse_reaction, Lines, RL),
    list_to_assoc(RL, Rs).

add_extra(E, X, E1, E2) :-
    (get_assoc(X, E1, N) -> plus(N, E, N1); N1 = E),
    put_assoc(X, E1, N1, E2).

add_req(_, [], R, R).
add_req(R, [(X, N) | Xs], R1, R2) :-
    RN is R*N, (get_assoc(X, R1, N1) -> plus(RN, N1, N2); N2 = RN),
    put_assoc(X, R1, N2, RT),
    add_req(R, Xs, RT, R2).

iterate_once(X, R1, R2, E1, E2, Data) :-
    get_assoc(X, R1, N),
    (get_assoc(X, E1, NE) -> N1 is N - NE; N1 = N),% Assume +ve N1
    put_assoc(X, E1, 0, ET),
    get_assoc(X, Data, (M, Cs)),
    R is ceiling(N1/M), E is R*M -N1, %TODO: take from extra too
    del_assoc(X, R1, _, RT),
    add_req(R, Cs, RT, R2), % add requirements
    add_extra(E, X, ET, E2). % add extra

solve((R, E), (R, E), _) :- assoc_to_list(R, ['ORE'-_]).
solve((R1, E1), (R2, E2), Data) :-
    gen_assoc(X, R1, _), X \= 'ORE',
    iterate_once(X, R1, RT, E1, ET, Data),
    solve((RT, ET), (R2, E2), Data).
part1(File, N) :-
    read_reactions(File, Data), empty_assoc(E),
    list_to_assoc(['FUEL'-1], Start), solve((Start, E), (X, _), Data),
    get_assoc('ORE', X, N).

get_n_fuel(File, N, O) :-
    read_reactions(File, Data), empty_assoc(E),
    list_to_assoc(['FUEL'-N], Start), solve((Start, E), (X, _), Data),
    get_assoc('ORE', X, O).

bin_search_fuel(S, E, File, O, N) :-
    plus(S, 1, E) -> N = S;
    M is ceiling((S+E)/2),
    get_n_fuel(File, M, OM),
    (OM < O -> bin_search_fuel(M, E, File, O, N);
     bin_search_fuel(S, M, File, O, N)).

part2(File, N) :-
    % Assuming efficiency wont be more than 10 times(2 would've possibly sufficed)
    part1(File, X), Y is floor(1_000_000_000_000/X), Z is Y*10,
    bin_search_fuel(Y, Z, File, 1_000_000_000_000, N).
