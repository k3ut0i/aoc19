% An answer set solution for this constraint problem
digit(1..6, 0..9).
cns(I, J) :- digit(I, A), digit(J, B), A = B.
num(X) :- digit(1, D1), digit(2, D2), digit(3, D3),
			 		digit(4, D4), digit(5, D5), digit(6, D6),
					1{cns(D1,D2); cns(D2, D3); cns(D3, D4); cns(D4,D5); cns(D5, D6)},
					D1 >= D2, D2 >= D3, D3 >= D4, D4 >= D5, D5 >= D6,
					X = D1 + 10*D2 + 100*D3 + 1000*D4 + 10000*D5 + 100000*D6,
					X >= 108457, X <= 562041.

ans(N) :- N = #count { X : num(X) }.


#show ans/1.
