% -*- mode: prolog -*-
:- module(utils, [difference/3]).

remove(Elem, List, Result) :-
    remove_(Elem, List, Result, []).
remove_(_, [], R, Acc) :- reverse(R, Acc).
remove_(X, [Y | Xs], R, Acc) :-
    (X = Y -> remove_(X, Xs, R, Acc);
     remove_(X, Xs, R, [Y | Acc])).

    
difference(L1, L2, D) :-
    difference_(L1, L2, D).
difference_([], _, []).
difference_(Xs, [], Xs).
difference_(Xs, [Y | Ys], D) :-
    remove(Y, Xs, Xs1),
    difference_(Xs1, Ys, D).
