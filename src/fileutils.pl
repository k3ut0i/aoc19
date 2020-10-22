%% -*- mode : prolog -*-
:- module(fileutils, [read_all_lines/2]).

read_all_lines(File, Lines) :-
    open(File, read, Stream),
    read_all_lines_(Stream, Lines, []),
    close(Stream).

read_all_lines_(Stream, Lines, Acc) :-
    read_line_to_string(Stream, Line),
    (Line = end_of_file -> reverse(Lines, Acc);
     read_all_lines_(Stream, Lines, [Line | Acc])).

