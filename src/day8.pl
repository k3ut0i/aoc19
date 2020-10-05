%% -*- mode : prolog -*-
digit_code(D, C) :- number_codes(D, [C]).
read_digits(File, Ds) :-
    read_file_to_codes(File, CsEOF, []), select(10, CsEOF, Cs),
    maplist(digit_code, Ds, Cs).
take(L, N, H, R) :- length(H, N), append(H, R, L).
splice_image([], [], _).
splice_image(Digits, [Layer | Layers], (W, H)) :-
    LayerSize is W*H, take(Digits, LayerSize, Layer, Rest),
    splice_image(Rest, Layers, (W, H)).
noOfDigits(Digit, Layer, N) :-
    findall(X, (X = Digit,member(X, Layer)), Xs), length(Xs, N).
calculateDigest(Layers, Ans) :- calculateDigest(Layers, Ans, []).
calculateDigest([], Ans, LL) :- find_min_1((_, Ones, Twos), LL),
				Ans is Ones * Twos.
calculateDigest([L | Ls], Ans, LL) :-
    noOfDigits(0, L, Zeros), noOfDigits(1, L, Ones), noOfDigits(2, L, Twos),
    calculateDigest(Ls, Ans, [(Zeros, Ones, Twos) | LL]).

find_min_1((A, B, C), [(A, B, C)]).
find_min_1((A, B, C), [(D, E, F) | Rest]) :-
    find_min_1((D1, E1, F1), Rest),
    (D < D1 -> A = D, B = E, C = F;
     A = D1, B = E1, C = F1).
part1(File, Size, Ans) :-
    read_digits(File, Digits), splice_image(Digits, Layers, Size),
    calculateDigest(Layers, Ans).

layer_pixel([V], V).
layer_pixel([2 |Ps], V) :- layer_pixel(Ps, V).
layer_pixel([0 | _], 0).
layer_pixel([1 | _], 1).

cons(X, Xs, [X | Xs]).
zipList(_, [], [], []).
zipList(F, [X | Xs], [Y | Ys], [Z | XYs]) :-
    zipList(F, Xs, Ys, XYs), call(F, X, Y, Z).
zip([[]], []).
zip([[X | Xs]], [[X] | Xss]) :- zip([Xs], Xss).
zip([[X |Xs] | Xss], [[X | Z] | ZXs]) :-
    zip(Xss, [Z | Zs]), zipList(cons, Xs, Zs, ZXs).


layer_image(Layers, Image) :-
    zip(Layers, ZL), maplist(layer_pixel, ZL, Image).
    
draw_image((W, H), Bits, File) :-
    open(File, write, S),
    atomics_to_string(Bits, " ", Data),
    format(S, "P1~n~d ~d~n~s~n", [W, H, Data]),
    close(S).
part2(Size, DataFile, ImageFile) :-
    read_digits(DataFile, Digits), splice_image(Digits, Layers, Size),
    layer_image(Layers, Image), draw_image(Size, Image, ImageFile).
