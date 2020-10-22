%% -*- mode : prolog -*-
%% Day 10 : Monitoring Station
:- use_module(fileutils).
:- use_module(utils).

%% TODO: X is Row index??
lookup((X, Y), Map, Elem) :-
    nth0(X, Map, Row), nth0(Y, Row, Elem).

pos_code('#', asteroid).
pos_code('.', empty).
read_map_line(Line, Row) :-
    atom_chars(Line, Chars), maplist(pos_code, Chars, Row).
get_size([L | Ls], (W, H)) :-
    length([L | Ls], H), length(L, W).
read_map(File, Map, (W, H)) :-
    read_all_lines(File, Lines),
    maplist(read_map_line, Lines, Map),
    get_size(Map, (W, H)).
%write_map(File, Map) :-
    
%point_on_line(From, _, From).
%point_on_line(_, To, To).
point_on_line((FX, FY), (TX, TY), (X, Y)) :-
    plus(FX, XD1, TX), plus(FY, YD1, TY),
    plus(FX, XD2, X), plus(FY, YD2, Y),
    T is YD1*XD2, T is YD2*XD1.

between_n(F, T, X) :-
    (F =< T -> between(F, T, X); between(T, F, X)).

points_in_line((FX, FY), (TX, TY), Points) :-
    findall((X,Y),
	    (between_n(FX, TX, X), between_n(FY, TY, Y),
	     point_on_line((FX, FY), (TX, TY), (X, Y))),
	    Points).
line((FX, FY), (TX, TY), (W, H), Line) :-
    succ(W1, W), succ(H1, H),
    findall((X,Y),
	    (between(0, W1, X), between(0, H1, Y),
	     point_on_line((FX, FY), (TX, TY), (X, Y))),
	    Line).

lines_through((X, Y), (W, H), Lines) :-
    succ(W1, W), succ(H1, H),
    findall((TX, TY),
	    (between(0, W1, TX), between(0, H1, TY)),
	    Points),
    select((X, Y), Points, Rem),
    lines_through_((X, Y), (W, H), Rem, Lines, []).
lines_through_(_, _, [], Lines, Lines).
lines_through_((X, Y), (W, H), [(PX, PY) | Ps], Lines, Acc) :-
    line((X, Y), (PX, PY), (W, H), Line),
    difference([(PX, PY) | Ps], Line, Points), %should have used subtract, but could not find it then.
    lines_through_((X, Y), (W, H), Points, Lines, [Line | Acc]).

asteroids_seen_from((X, Y), (W, H), Map, N) :-
    lines_through((X, Y), (W, H), Lines),
    asteroids_seen_from_((X, Y), (W, H), Map, Lines, N).

asteroids_seen_from_(_, _, _, [], 0).
asteroids_seen_from_((X, Y), (W, H), Map, [Line | Lines], N) :-
    asteroids_seen_on((X, Y), Line, Map, N1),
    asteroids_seen_from_((X, Y), (W, H), Map, Lines, N2),
    plus(N1, N2, N).

asteroids_seen_on((X, Y), Line, Map, N) :-
    append(S1, [(X, Y) | S2], Line),
    asteroids_seen_along((X, Y), S1, Map, N1),
    asteroids_seen_along((X, Y), S2, Map, N2),
    plus(N1, N2, N).
asteroids_seen_along(_, [], _, 0).
asteroids_seen_along((X, Y), [(PX, PY) | Ps], Map, N) :-
    lookup((PX, PY), Map, asteroid) -> N = 1;
    asteroids_seen_along((X, Y), Ps, Map, N).


find_asteroids_views(Map, Size, AMap) :-
    findall((X, Y), lookup((X, Y), Map, asteroid), As),
    bagof(((X, Y), N),
	  (member((X, Y), As),
	   asteroids_seen_from((X, Y), Size, Map, N)), AMap).
    
find_max_asteroid(AMap, A) :- nth0(0, AMap, F), find_max_(AMap, A, F).
find_max_([], A, A).
find_max_([(P, N) | Xs], A, (P1, N1)) :-
    N < N1 -> find_max_(Xs, A, (P1, N1));
    find_max_(Xs, A, (P, N)).

%lines_through_point(Lines, (PX, PY), (W, H)) :-   between(0, W, X), between(0, H, Y).
    
    
%write_map

