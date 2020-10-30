%% -*- mode: prolog -*-

% pattern for the nth digit mth-coeffcient
digit_m(N, M, D) :-
    divmod(M, N, Q, _),  divmod(Q, 4, _, C),
    (C = 0, D = 0; C = 1, D = 1; C = 2, D = 0; C = 3, D = -1).

calculate_digit_n(N, In, D) :-
    calculate_digit_n_(N, In, D, 1, 0).
calculate_digit_n_(_, [], D, _, Acc) :- D1 is abs(Acc), divmod(D1, 10, _, D).
calculate_digit_n_(N, [I | Is], D, M, Acc) :-
    once(digit_m(N, M, C)), P is C*I, M1 is M+1, Acc1 is Acc+P,
    calculate_digit_n_(N, Is, D, M1, Acc1).

phase_step(In, Out) :-
    length(In, L), L1 is L + 1, phase_step_(In, Out, L1, 1, []).
phase_step_(_, Out, L, L, Acc) :- reverse(Out, Acc).
phase_step_(In, Out, L, N, Acc) :-
    N < L, once(calculate_digit_n(N, In, D)), N1 is N + 1,
    phase_step_(In, Out, L, N1, [D | Acc]).

step_n(0, I, I).
step_n(N, In, Out) :-
    N > 0, N1 is N - 1, once(phase_step(In, T)),
    step_n(N1, T, Out).

% verbatim from day16.pl    
number_to_digits(N, [N | L], L) :- N < 10.
number_to_digits(N, L, Acc) :-
    N >= 10, divmod(N, 10, Q, R),
    number_to_digits(Q, L, [R | Acc]).
digits_to_number(Ds, N) :- digits_to_number(Ds, N, 0).
digits_to_number([], A, A).
digits_to_number([D | Ds], N, A) :-
    A1 is A*10+D, digits_to_number(Ds, N, A1).
after_M_phases(M, N, E) :-
    number_to_digits(N, D, []), step_n(M, D, D1),
    append(E, _, D1), length(E, 8).
%% after_M_phases_FILE(0, _).
%% after_M_phases_FILE(M, File) :-
%%     M > 0, plus(M1, 1, M),
%%     open(File, read, S1), read_line_to_string(S1, S), close(S1),
%%     number_string(N, S), number_to_digits(N, D, []),
%%     phase_step(D, D1), digits_to_number(D1, N1),
%%     open(File, write, S2), write(S2, N1), close(S2),
%%     after_M_phases_FILE(M1, File).
part1(File, Ans) :-
    open(File, read, Stream),
    read_line_to_string(Stream, S),
    number_string(N, S),
    after_M_phases(100, N, Ans).
