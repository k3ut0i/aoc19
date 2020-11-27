% An answer set solution for this constraint problem
digit(1..6, 0..9).
d(0..9).
eqDigit(I, J) :- d(I), d(J), I = J.
num(X) :- digit(1, D1), digit(2, D2), digit(3, D3),
			 		digit(4, D4), digit(5, D5), digit(6, D6),
					1{eqDigit(D1,D2); eqDigit(D2, D3); eqDigit(D3, D4); eqDigit(D4,D5); eqDigit(D5, D6)},
					D1 >= D2, D2 >= D3, D3 >= D4, D4 >= D5, D5 >= D6,
					X = D1 + 10*D2 + 100*D3 + 1000*D4 + 10000*D5 + 100000*D6,
					X >= 108457, X <= 562041.

ans(N) :- N = #count { X : num(X) }.

%% Part2
%% Now I need assurance that there is a sequence with just length two 
%% D1 = D2 != D3 or D4 != D5 = D6 or Di != Di1 = Di2 != Di3.
justTwoEq(D1, D2, D3, D4, D5, D6) :-
  d(D1), d(D2), d(D3), d(D4), d(D5), d(D6),
	D1 = D2, not D2 = D3.
justTwoEq(D1, D2, D3, D4, D5, D6) :-
  d(D1), d(D2), d(D3), d(D4), d(D5), d(D6),
	not D1 = D2, D2 = D3, not D3 = D4.
justTwoEq(D1, D2, D3, D4, D5, D6) :-
  d(D1), d(D2), d(D3), d(D4), d(D5), d(D6),
	not D2 = D3, D3 = D4, not D4 = D5.
justTwoEq(D1, D2, D3, D4, D5, D6) :-
  d(D1), d(D2), d(D3), d(D4), d(D5), d(D6),
	not D3 = D4, D4 = D5, not D5 = D6.
justTwoEq(D1, D2, D3, D4, D5, D6) :-
  d(D1), d(D2), d(D3), d(D4), d(D5), d(D6),
	not D4 = D5, D5 = D6.

num1(X) :- digit(1, D1), digit(2, D2), digit(3, D3),
			 		digit(4, D4), digit(5, D5), digit(6, D6),
					1{justTwoEq(D1, D2, D3, D4, D5, D6)},
					D1 >= D2, D2 >= D3, D3 >= D4, D4 >= D5, D5 >= D6,
					X = D1 + 10*D2 + 100*D3 + 1000*D4 + 10000*D5 + 100000*D6,
					X >= 108457, X <= 562041.

ans1(N) :- N = #count { X : num1(X)}.
#show ans/1.
#show ans1/1.
