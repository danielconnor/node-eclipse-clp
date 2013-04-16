% ----------------------------------------------------------------------
% System:       ECLiPSe Constraint Logic Programming System
% Copyright (C) Parc Technologies Ltd. 2001
% Version:      
%
% Example program for using queues with JavaInterface module 
%
% Author:       Josh Singer, Parc Technologies Ltd. 
% ----------------------------------------------------------------------

read_5_write_5:-
	count(I, 1, 5) do 
           (read_exdr(java_to_eclipse, TermIn),
	    write_exdr(eclipse_to_java, foo(TermIn, bar)),
	    flush(eclipse_to_java)).
    
