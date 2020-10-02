%% -*- mode: prolog -*-
sanitize_directions(I, O) :- sanitize_directions(I, O, []).
sanitize_directions([], Out, Acc) :- reverse(Out, Acc).
sanitize_directions([I | Is], Out, Acc) :-
    sanitize_directions(Is, Out, [D | Acc]), parse_direction(I, D).

convert_dir('R', right). convert_dir('L', left). convert_dir('U', up).
convert_dir('D', down).

parse_direction(IStr, Dir) :- string_chars(IStr, [DChar | DisL]),
			      number_chars(Dis, DisL),
			      convert_dir(DChar, D),
			      Dir = (D, Dis).
draw_step((X, Y), Step, Path, NewPos) :-
    Step = (right, N), Xnew is X+N, NewPos=(Xnew, Y),
    bagof((Xt, Y), between(X, Xnew, Xt) , Path);
    Step = (left, N), Xnew is X-N, NewPos=(Xnew, Y),
    bagof((Xt, Y), between(Xnew, X, Xt), Path);
    Step = (up, N), Ynew is Y+N, NewPos=(X, Ynew),
    bagof((X, Yt), between(Y, Ynew, Yt), Path);
    Step = (down, N), Ynew is Y-N, NewPos=(X, Ynew),
    bagof((X, Yt), between(Ynew, Y, Yt), Path).

draw_path(CurrentPos, Dirs, Path) :- draw_path(CurrentPos, Dirs, Path, []).
draw_path(_, [], Path, Acc) :- Path=Acc.
draw_path(CurrentPos, [S | Ss], Path, Acc) :-
    draw_step(CurrentPos, S, Trail, NPos),
    append(Trail, Acc, NewAcc),
    draw_path(NPos, Ss, Path, NewAcc).

manhat_dis((X, Y), Z) :- X1 is abs(X), Y1 is abs(Y), Z is X1+Y1.
find_smallest([P | Ps], S) :- find_smallest(Ps, P, S).
find_smallest([], Current, Smallest) :- Smallest = Current.
find_smallest([P | Ps], C, S) :-
    manhat_dis(P, Pdis), manhat_dis(C, Cdis),
    (Pdis <  Cdis -> find_smallest(Ps, P, S); find_smallest(Ps, C, S)).
find_nearest_intersection(P1, P2, Pos) :-
    intersection(P1, P2, Ps0), select((0, 0), Ps0, Ps), find_smallest(Ps, Pos).
get_nearest(P1Str, P2Str, P) :-
    atomics_to_string(P1A, ",", P1Str),atomics_to_string(P2A, ",", P2Str),
    sanitize_directions(P1A, P1), sanitize_directions(P2A, P2),
    draw_path((0, 0), P1, Path1), draw_path((0, 0), P2, Path2),
    find_nearest_intersection(Path1, Path2, P).

test_cases(P) :-
    Cases = [("R75,D30,R83,U83,L12,D49,R71,U7,L72",
	      "U62,R66,U55,R34,D71,R55,D58,R83"),
	     ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
	      "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")],
    member((P1, P2), Cases), get_nearest(P1, P2, P).
    
part1(File, Ans) :- open(File, read, Stream),
		    read_line_to_string(Stream, P1Str),
		    read_line_to_string(Stream, P2Str),
		    close(Stream),
		    get_nearest(P1Str, P2Str, Ans).
