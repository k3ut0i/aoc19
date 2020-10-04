%% -*- mode: prolog -*-
:- use_module(library(simplex)).
read_all_lines(File, Lines) :-
    open(File, read, Stream),
    read_all_lines_(Stream, Lines, []),
    close(Stream).
read_all_lines_(Stream, Lines, Acc) :-
    read_line_to_string(Stream, Line),
    (Line = end_of_file -> reverse(Acc, Lines);
     read_all_lines_(Stream, Lines, [Line | Acc])).
parse_line(L, Orbit) :-
    string_codes(L, [A1, A2, A3, 41, B1, B2, B3]), %% 41 = ')'
    atom_codes(A, [A1, A2, A3]), atom_codes(B, [B1, B2, B3]),
    Orbit = (A, B).
get_orbits(File, Orbits) :-
    read_all_lines(File, Lines), maplist(parse_line, Lines, Orbits).
    
%% print_orbit(Stream, (A, B)) :-
%%     format(Stream, "orbits('~a', '~a').~n", [B, A]).
%% print_all_orbits(_, []).
%% print_all_orbits(Stream, [O | Os]) :-
%%     print_orbit(Stream, O), print_all_orbits(Stream, Os).
%% print_orbits(InFile, OutFile) :-
%%     read_all_lines(InFile, Lines), maplist(parse_line, Lines, Orbits),
%%     open(OutFile, write, Stream),
%%     print_all_orbits(Stream, Orbits),
%%     close(Stream).
draw_all_orbits(_, []).
draw_all_orbits(Stream, [(A, B) | Os]) :-
    format(Stream, "  <~a> -> <~a>;~n", [B, A]),
    draw_all_orbits(Stream, Os).
draw_orbits(InFile, OutFile) :-
    read_all_lines(InFile, Lines), maplist(parse_line, Lines, Orbits),
    open(OutFile, write, Stream),
    format(Stream, "digraph Orbits {~n", []),
    format(Stream, "  node [shape=point];~n", []),
    format(Stream, "  edge [arrowhead=none];~n", []),
    graph_draw_path(Stream, 'YOU', Orbits, 'green'),
    graph_draw_path(Stream, 'SAN', Orbits, 'red'),
    draw_all_orbits(Stream, Orbits),
    format(Stream, "}~n", []),
    close(Stream).

find_origin(Orbits, X) :-
    member((X, _), Orbits), \+(member((_, X), Orbits)).

rank(Orbits, Ranks) :-
    empty_assoc(A), find_origin(Orbits, O), put_assoc(O, A, 0, AO),
    rank(Orbits, Ranks, AO).

set_rank(Planet, Rank, Ranks, NewRanks) :-
    put_assoc(Planet, Ranks, Rank, NewRanks).
set_rank_all([], _, Rs, Rs).
set_rank_all([P | Ps], R, Rs, NRs) :-
    set_rank(P, R, Rs, T),
    set_rank_all(Ps, R, T, NRs).
rank([], _, R, R).
rank([P | Ps], Orbits, Old, New) :-
    findall(O, member((P, O), Orbits), Os),
    get_assoc(P, Old, R), plus(R, 1, RN),
    set_rank_all(Os, RN, Old, T), append(Os, Ps, NewPs),
    rank(NewPs, Orbits, T, New).
rank_all(Orbits, Ranks) :-
    empty_assoc(A), find_origin(Orbits, O), set_rank(O, 0, A, AO),
    rank([O], Orbits, AO, Ranks).
sum_ranks(Ranks, Sum) :-
    findall(R, gen_assoc(_, Ranks, R), Rs), foldl(plus, Rs, 0, Sum).
part1(File, Ans) :- get_orbits(File, Orbits), rank_all(Orbits, Ranks),
		    sum_ranks(Ranks, Ans).

find_path('COM', _, []).
find_path(P, Orbits, Path) :-
    member((O, P), Orbits), find_path(O, Orbits, Tail), Path = [O | Tail].
write_dis(_, [], []).
write_dis(C, [P | Ps], [(P, C) | Ds]) :- plus(C, 1, C1), write_dis(C1, Ps, Ds).
    
find_dis(Orbits, P) :-
    find_path('SAN', Orbits, SP), find_path('YOU', Orbits, YP),
    write_dis(0, SP, SDis), write_dis(0, YP, YDis),
    findall(D, (member((P, PS), SDis), member((P, PY), YDis), D is PS+PY), Ds),
    min_list(Ds, P).
part2(File, Ans) :- get_orbits(File, Os), find_dis(Os, Ans).

draw_node(Stream, P, Color) :-
    format(Stream, '<~a> [color=~a];~n', [P, Color]).
draw_nodes(_, [], _).
draw_nodes(Stream, [P | Ps], C) :-
    draw_node(Stream, P, C), draw_nodes(Stream, Ps, C).
graph_draw_path(Stream, P, Orbits, Color) :-
    draw_node(Stream, P, Color),
    find_path(P, Orbits, Path),
    draw_nodes(Stream, Path, Color).
