% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: http_method.pl,v 1.5 2001/09/13 17:48:56 js10 Exp $
% ----------------------------------------------------------------------


/*
$Id: http_method.pl,v 1.5 2001/09/13 17:48:56 js10 Exp $
*/

:- module(http_method).

:- export
        http_method/6.


/* 
rpc call:
executes the method on the object and return:
- the output of the method (possibly empty)
- a status code for the response status line
- a list of http parameters (in particular the length of the object body).

*/


http_method(_,_,_,_,_,_):-
	http_method(_,_,_,_,_,_).
http_method(Method, Url, _ObjectBody, Output, 200, [contentLength(CL)]):-
	concat_string(["You asked for method ", Method, 
	"on object ", Url], Output),
	string_length(Output, CL).
	

	
