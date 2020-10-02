%% -*- mode : prolog -*-

digit(D) :- between(0, 9, D).
find_num(Num) :-
    digit(A0), digit(A1), digit(A2), digit(A3), digit(A4), digit(A5),
    A0 >= A1, A1 >= A2, A2 >= A3, A3 >= A4, A4 >= A5,
    (A0 = A1; A1 = A2; A2 = A3; A3 = A4; A4 = A5),
    Num is A0 + 10*A1 + 100*A2 + 1000* A3 + 10000*A4 + 100000*A5,
    Num >= 108457, Num =< 562041.
find_num2(Num) :-
    digit(A0), digit(A1), digit(A2), digit(A3), digit(A4), digit(A5),
    A0 >= A1, A1 >= A2, A2 >= A3, A3 >= A4, A4 >= A5,
    (A0 = A1, A1 \= A2;
     A0 \= A1, A1 = A2, A2 \= A3;
     A1 \= A2, A2 = A3, A3 \= A4;
     A2 \= A3, A3 = A4, A4 \= A5;
     A3 \= A4, A4 = A5),
    Num is A0 + 10*A1 + 100*A2 + 1000* A3 + 10000*A4 + 100000*A5,
    Num >= 108457, Num =< 562041.
part1(Ans) :-
    setof(X, find_num(X), Xs), length(Xs, Ans).
part2(Ans) :-
    setof(X, find_num2(X), Xs), length(Xs, Ans).

