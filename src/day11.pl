%% -*- mode : prolog -*-
:- use_module(intcode).

%% Encode directions with Up-0, Right-1, Down-2, Left-3. Clockwise manner.
robot_step(T, (D, (X, Y)), (ND, (NX, NY))) :-
    Change is 2*T - 1, ND is (D + Change) mod 4,
    (ND = 0, NX = X, plus(NY, 1, Y);
     ND = 1, plus(X, 1, NX), NY = Y;
     ND = 2, NX = X, plus(Y, 1, NY);
     ND = 3, plus(NX, 1, X), NY = Y).

color_hull(Paint, (_, (X, Y)), W1, W2) :-
    member((X, Y, _), W1) -> select((X, Y, _), W1, (X, Y, Paint), W2);
    W2 = [(X, Y, Paint) | W1].

run_robot_once((IP1, M1, B1, P1, W1), (IP2, M2, B2, P2, W2), S) :-
    P1 = (_, X, Y), (member((X, Y, Color), W1); Color = 0),
    run_program_async((IP1, M1, B1), Color, (IP2, M2, B2), [Paint, Turn], S),
    robot_step(Turn, P1, P2),
    color_hull(Paint, P1, W1, W2).

run_robot(Prog, World) :-
    run_robot_((0, Prog, 0, (0, 0, 0), []), (_, _, _, _, World)).
run_robot_(In, Out) :-
    run_robot_once(In, T, S),
    (S = s -> Out = T; run_robot_(T, Out)).
run_robot_n(Prog, World, N) :-
    run_robot_n_((0, Prog, 0, (0, 0, 0), []), (_, _, _, _, World), N).
run_robot_n_(_, _, 0).
run_robot_n_(In, Out, N) :-
    N >= 0, run_robot_once(In, T, _),
    N1 is N -1, run_robot_n_(T, Out, N1).

get_unique_pos(File, N) :-
    csv_read_file(File, Rows),
    findall((X, Y), member(row(_, X, Y), Rows), Psd),
    setof(P, member(P, Psd), Ps), length(Ps, N).

%% Messed up here. I need to provide the color of the
%% current position. Here I just keep giving it black.
%% TODO: I need to create an environment that steps through
%% the program after each move and senses the color.
%% So maintain the list of white panels and if the bot is at it
%% input 1 or else input 0.
get_outputs(File, O) :-
    get_program(File, P), run_program((0, P, 0), 0, _ , O).

robot_trail(Outputs, Trail) :-
    robot_trail_(Outputs, (0, (0, 0)), Trail, [(0, (0, 0))]).
robot_trail_([], _, Trail, Acc) :- reverse(Trail, Acc).
robot_trail_([_ | [Turn | Rest]], RS, Trail, Acc) :-
    robot_step(Turn, RS, RSNew),
    robot_trail_(Rest, RSNew, Trail, [RSNew | Acc]).

get_pos((_, (X, Y)), (X, Y)).
get_trail_pos(Ts, Ps) :- maplist(get_pos, Ts, P),
			 setof(X, member(X, P), Ps).

get_size(Trail, Size) :-
    Trail = [(_, X, Y) | _], get_size(Trail, X, Y, X, Y, Size).

get_size([], MinX, MinY, MaxX, MaxY, (X, Y)) :-
    X is MaxX - MinX, Y is MaxY - MinY.
get_size([(_,(PX, PY)) | Ts], MinX, MinY, MaxX, MaxY, Size) :-
    MinXN is min(PX, MinX), MinYN is min(PY, MinY),
    MaxXN is max(PX, MaxX), MaxYN is max(PY, MaxY),
    get_size(Ts, MinXN, MinYN, MaxXN, MaxYN, Size).
    
%% draw_trail(File, Trail) :-
%%     open(File, write, Stream),
%%     close(Stream).
