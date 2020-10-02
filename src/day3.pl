%% -*- mode : prolog -*-
sanitize_directions(IStr, O) :-
    atomics_to_string(I, ",", IStr), sanitize_directions(I, O, []).
sanitize_directions([], Out, Acc) :- reverse(Out, Acc).
sanitize_directions([I | Is], Out, Acc) :-
    sanitize_directions(Is, Out, [D | Acc]), parse_direction(I, D).

convert_dir('R', right). convert_dir('L', left). convert_dir('U', up).
convert_dir('D', down).

parse_direction(IStr, Dir) :- string_chars(IStr, [DChar | DisL]),
			      number_chars(Dis, DisL),
			      convert_dir(DChar, D),
			      Dir = (D, Dis).
line((X, _), (X, _)).
line((_, Y), (_, Y)).

get_line((X, Y), Step, Line, NewPos) :-
    Step = (right, N), Xnew is X+N, NewPos=(Xnew, Y),
    Line = ((X, Y), (Xnew, Y));
    Step = (left, N), Xnew is X-N, NewPos=(Xnew, Y),
    Line = ((X, Y), (Xnew, Y));
    Step = (up, N), Ynew is Y+N, NewPos=(X, Ynew),
    Line = ((X, Y), (X, Ynew));
    Step = (down, N), Ynew is Y-N, NewPos=(X, Ynew),
    Line = ((X, Y), (X, Ynew)).

get_lines(InitPos, Steps, Lines) :- get_lines(InitPos, Steps, Lines, []).
get_lines(_, [], Lines, Acc) :- reverse(Lines, Acc).
get_lines(InitPos, [S | Ss], Lines, Acc) :-
    get_line(InitPos, S, Line, NewPos),
    get_lines(NewPos, Ss, Lines, [Line | Acc]).

on_line((X, Y), ((X, Y1), (X, Y2)), S) :-
    between(Y1, Y2, Y), plus(Y1, S1, Y), S is abs(S1) ;
    between(Y2, Y1, Y), plus(Y1, S1, Y), S is abs(S1).
on_line((X, Y), ((X1, Y), (X2, Y)), S) :-
    between(X1, X2, X), plus(X1, S1, X), S is abs(S1) ;
    between(X2, X1, X), plus(X1, S1, X), S is abs(S1).

line_length(((X, Y1), (X, Y2)), Len) :- plus(Y1, L, Y2), Len is abs(L).
line_length(((X1, Y), (X2, Y)), Len) :- plus(X1, L, X2), Len is abs(L).

intersects(((X, Y1), (X, Y2)), ((X1, Y), (X2, Y)), P) :-
    (between(X1, X2, X) ; between(X2, X1, X)),
    (between(Y1, Y2, Y) ; between(Y2, Y1, Y)), P = (X, Y).
intersects(((X1, Y), (X2, Y)), ((X, Y1), (X, Y2)), P) :-
    intersects(((X, Y1), (X, Y2)), ((X1, Y), (X2, Y)), P).

manhat_dis((X, Y), Z) :- X1 is abs(X), Y1 is abs(Y), Z is X1+Y1.
find_nearest([P | Ps], S) :- find_nearest(Ps, P, S).
find_nearest([], Current, Smallest) :- Smallest = Current.
find_nearest([P | Ps], C, S) :-
    manhat_dis(P, Pdis), manhat_dis(C, Cdis),
    (Pdis <  Cdis -> find_nearest(Ps, P, S); find_nearest(Ps, C, S)).

count_steps(P, L, S) :- count_steps(P, L, S, 0).
count_steps(Point, [L | Ls], Steps, Acc) :-
    (on_line(Point, L, Current), Steps is Acc + Current) ;
    (line_length(L, Current),Anew is Acc + Current,
     count_steps(Point, Ls, Steps, Anew)).

main(File, Ans1, Ans2) :-
    open(File, read, Stream),
    read_line_to_string(Stream, P1Str), read_line_to_string(Stream, P2Str),
    close(Stream), solve(P1Str, P2Str, Ans1, Ans2).

solve(P1Str, P2Str, ManAns, StepAns) :-
    sanitize_directions(P1Str, P1Dir), sanitize_directions(P2Str, P2Dir),
    get_lines((0, 0), P1Dir, Ls1), get_lines((0, 0), P2Dir, Ls2),
    bagof(P, L1^L2^(member(L1, Ls1), member(L2, Ls2), intersects(L1, L2, P)), Ps0),
    find_nearest(Ps0, ManAns),
    bagof(Y, X^S1^S2^(member(X, Ps0),
    		      count_steps(X, Ls1, S1),
    		      count_steps(X, Ls2, S2),
    	      Y is S1 + S2),
    	  Steps),
    min_list(Steps, StepAns).

