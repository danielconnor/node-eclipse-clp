% ----------------------------------------------------------------------
% System:       ECLiPSe Constraint Logic Programming System
% Copyright (C) Parc Technologies Ltd. 2001
% Version:      
%
% Example program for using queues with JavaInterface module 
%
% Author:       Josh Singer, Parc Technologies Ltd. 
% ----------------------------------------------------------------------

read_2_write_1:-
	read_exdr(java_to_eclipse, Term1),
	read_exdr(java_to_eclipse, Term2),
	write_exdr(eclipse_to_java, pair(Term1, Term2)),
	flush(eclipse_to_java).







