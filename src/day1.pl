%% -*- mode:prolog -*-
read_number(N, Stream) :- read_line_to_codes(Stream, X),
			  ( X = end_of_file -> N = X; number_codes(N, X)).

read_all_numbers(Stream, Acc, Total) :- read_number(N, Stream),
					(N = end_of_file -> Acc = Total;
					 read_all_numbers(Stream, [N | Acc], Total)).

fuel_req(Mass, Req) :- divmod(Mass, 3, X, _), plus(2, Req, X).

% Rounding is confusing. Why should each modules fuel be dealt sperately?
% After counting fuel of all the modules, we should have calculated the
% additional fuel required to transport it.
count_fuel(MassArray, Fuel) :- maplist(fuel_req, MassArray, FuelArray),
			       maplist(total_fuel, FuelArray, TotalFuelArray),
			       foldl(plus ,TotalFuelArray, 0, Fuel).

main(File, Ans) :- open(File, read, In),
		   read_all_numbers(In, [], ModuleMasses),
		   count_fuel(ModuleMasses, Ans).

%additional_fuel(M, F) :- (M < 3 -> F = 0; fuel_req(M, F)).
total_fuel(M, F) :- total_fuel(M, 0, F).
total_fuel(M, Acc, F) :- (M =< 0 -> F = Acc;
			  fuel_req(M, X),
			  plus(M, Acc, N),
			  total_fuel(X, N, F)).
