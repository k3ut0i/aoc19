%% -*- mode : prolog -*-
%% Day 10 : Monitoring Station
:- use_module(fileutils).
:- use_module(utils).

%% TODO: X is Row index??
lookup((X, Y), Map, Elem) :-
    nth0(X, Map, Row), nth0(Y, Row, Elem).
store((X, Y), Elem, Map, NMap) :-
    nth0(X, Map, Row), nth0(Y, Row, _, Rest),
    nth0(Y, NRow, Elem, Rest),
    nth0(X, Map, _, RestRows),
    nth0(X, NMap, NRow, RestRows).
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

part1(File, A) :-
    read_map(File, Map, Size), find_asteroids_views(Map, Size, AsteroidV),
    find_max_asteroid(AsteroidV, A).

%% split the LINES through POS at POS into SL.
split_lines(Pos, Lines, SL) :- split_lines_(Pos, Lines, SL, []).
split_lines_(_, [], SL, SL).
split_lines_(Pos, [L | Ls], SL, Acc) :-
    append(Prefix, [Pos | Suffix], L), reverse(Prefix, RP),
    P = [Pos | RP], S = [Pos | Suffix],
    (Prefix = [] -> split_lines_(Pos, Ls, SL, [S | Acc]);

     (Suffix = [] -> split_lines_(Pos, Ls, SL, [P | Acc]);
      split_lines_(Pos, Ls, SL, [P | [S | Acc]]))).

%% clock angle-
quadrant(Xsign, Ysign, Q) :-
    Xsign < 0, Ysign < 0, Q = top_left;
    Xsign < 0, Ysign > 0, Q = top_right;
    Xsign > 0, Ysign > 0, Q = bottom_right;
    Xsign > 0, Ysign < 0, Q = bottom_left;
    Xsign = 0, Ysign > 0, Q = right;
    Xsign = 0, Ysign < 0, Q = left;
    Xsign > 0, Ysign = 0, Q = bottom;
    Xsign < 0, Ysign = 0, Q = top.

angle((X1, Y1), (X2, Y2), A) :-
    XD is X2 - X1, YD is Y2 - Y1, quadrant(XD, YD, Q),
    (Q = top, A is 0;
     Q = top_right, A is -atan(YD/XD);
     Q = right, A is pi/2;
     Q = bottom_right, A is pi - atan(YD/XD);
     Q = bottom, A is pi;
     Q = bottom_left, A is pi - atan(YD/XD);
     Q = left, A is 3*pi/2;
     Q = top_left, A is 2*pi - atan(YD/XD)).

angle_of_line([P1 | [P2 | _]], A) :- angle(P1, P2, A).
compare_lines(D, L1, L2) :-
    angle_of_line(L1, A1), angle_of_line(L2, A2),
    compare(D, A1, A2).

%% Fails when asteroid cannot be found.    
asteroid([_InitP | Line], Map, A) :-
    member(A, Line), lookup(A, Map, asteroid).

step_laser(L, WorldIn, WorldOut, BombLog) :-
    asteroid(L, WorldIn, A) ->
	BombLog = A, store(A, empty, WorldIn, WorldOut);
    BombLog = empty, WorldIn = WorldOut.
    
rotate_laser(LaserPos, Size, WorldIn, WorldOut, BombLog) :-
    lines_through(LaserPos, Size, Lines),
    split_lines(LaserPos, Lines, SplitLines),
    predsort(compare_lines, SplitLines, SL),
    rotate_laser_(SL, WorldIn, WorldOut, BombLog).

%% Bad recursion.
rotate_laser_([], W, W, []).
rotate_laser_([L | Ls], WorldIn, WorldOut, BombLog) :-
    step_laser(L, WorldIn, WT, BT),
    rotate_laser_(Ls, WT, WorldOut, BR),
    BombLog = [BT | BR].

bombed_asteroids([], As, Acc) :- reverse(As, Acc).
bombed_asteroids([B | Bs], As, Acc) :-
    B = empty -> bombed_asteroids(Bs, As, Acc);
    bombed_asteroids(Bs, As, [B | Acc]).

nth_asteroid_bombed(1, [A | _], A).
nth_asteroid_bombed(N, [L | Ls], A) :-
    L = empty -> nth_asteroid_bombed(N, Ls, A);
    N > 1, plus(N1, 1, N), nth_asteroid_bombed(N1, Ls, A).

part2(File, Pos, A) :-
    read_map(File, Map, Size),
    rotate_laser(Pos, Size, Map, _, BombLog),
    bombed_asteroids(BombLog, ALog, []),
    nth1(200, ALog, A).
