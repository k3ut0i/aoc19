%% -*- mode: prolog -*-

repeat(_, [], []).
repeat(N, [X | Xs], L) :-
    repeat_(N, X, X1, []), repeat(N, Xs, Xs1),
    append(X1, Xs1, L).
repeat_(0, _, Acc, Acc).
repeat_(N, X, Xs, Acc) :-
    N > 0, plus(N1, 1, N), repeat_(N1, X, Xs, [X | Acc]).
%pattern(1, X) :- [_ | X] = [0, 1, 0, -1].
%pad list I with list L1 until it's length reaches N
pad_end(N, I, L1, L2) :-
    length(I, M),
    (N < M -> length(L2, N), prefix(L2, I)
    ;
    length(I, M), length(L1, M1), R is floor((N-M)/M1),
    reverse(I, IR), pad_end_(R, L1, L2T, IR),
    prefix(P, L1), append(L2T, P, L2), length(L2, N)).

pad_end_(0, _, L, LR) :- reverse(L, LR).
pad_end_(N, L1, L2, Acc) :-
    reverse(L1, L1R),
    N > 0, append(L1R, Acc, Acc1), N1 is N - 1,
    pad_end_(N1, L1, L2, Acc1).

pattern(N, L, P) :-
    repeat(N, [0, 1, 0, -1], Pbase), Pbase = [_ | Pstart],
    pad_end(L, Pstart, Pbase, P).

dot_product(In1, In2, Out) :-
    maplist([I1, I2, O]>>(O is I1*I2), In1, In2, Os),
    foldl(plus, Os, 0, Out).

last_digit_of(X, Y) :-
    X1 is abs(X), divmod(X1, 10, _, Y).

% Takes a lot of memory.
%% phase_step(In, Out) :-
%%     length(In, L), findall(X, between(1, L, X), NL),
%%     maplist({L}/[N1, O]>>pattern(N1, L, O), NL, Vs),
%%     maplist(dot_product(In), Vs, ON),
%%     maplist(last_digit_of, ON, Out).
phase_step(In, Out) :- length(In, L), phase_step_(L, In, Out).
phase_step_(N, In, Out) :- phase_step_(N, In, Out, []).
phase_step_(0, _, Out, Out).
phase_step_(N, In, Out, Acc) :-
    length(In, L), pattern(N, L, P), dot_product(In, P, A),
    last_digit_of(A, A1), plus(N1, 1, N), 
    phase_step_(N1, In, Out, [A1 | Acc]).

phase_n(0, I, I).
phase_n(N, In, Out) :-
%    print(N), nl,
    N > 0, phase_step(In, T), N1 is N - 1,
    phase_n(N1, T, Out).
    
number_to_digits(N, [N | L], L) :- N < 10.
number_to_digits(N, L, Acc) :-
    N >= 10, divmod(N, 10, Q, R),
    number_to_digits(Q, L, [R | Acc]).
digits_to_number(Ds, N) :- digits_to_number(Ds, N, 0).
digits_to_number([], A, A).
digits_to_number([D | Ds], N, A) :-
    A1 is A*10+D, digits_to_number(Ds, N, A1).

after_100_phases(N, E) :-
    number_to_digits(N, D, []), phase_n(100, D, D1),
    append(E, _, D1), length(E, 8).

part1(File, Ans) :-
    open(File, read, Stream),
    read_line_to_string(Stream, S),
    number_string(N, S),
    after_100_phases(N, Ans).
