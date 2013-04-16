%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   print.pl                                               %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pragma( expand).
:- pragma( nodebug).
:- local print_list/1.

print_linear_store( Vars) :-
	related_linear_vars( Vars, _, _, All),	% project.pl
	all_attribute_goals( All, Reps),
	( Reps = [_|_] ->
	    writeln( output, "\n\n% Linear constraints:\n{"),
	    print_list( Reps),
	    write( output, "\n}")
	;
	    true
	).

:- import printf_body/4 from sepia_kernel.

:- tool( printf_current/2, printf_current_body/3).
%
printf_current_body(Stream, Value, Module) :-
	get_flag(output_mode, Mode),
	concat_string(['%', Mode, 'w'], Format),
	printf_body(Stream, Format, [Value], Module).

print_list( []).
print_list( [X]) :- !,
	printf( output, "  ", []),
	printf_current( output, X).
print_list( [X|Xs]) :-
	printf( output, "  ", []),
	printf_current( output, X),
	printf( output, ',\n', []),
	print_list( Xs).
