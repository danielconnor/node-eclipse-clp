% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: http_server.pl,v 1.6 2004/10/27 14:32:37 ajs2 Exp $
% ----------------------------------------------------------------------

/*
    RPC using HTTP/1.0 (request status line) and
    MIME-Version:1.0 (request general header)

    SERVER 

    http document used is HTTP/1.0 from the Network Working Group
    - internet draft - expiring in june, 19 1995.
*/

:- module(http_server).

:- comment(summary, "HTTP server library").
:- comment(author, "Ph. Bonnet, S. Bressan and M. Meier, ECRC Munich").
:- comment(copyright, "Imperial College London and ICL").
:- comment(date, "$Date: 2004/10/27 14:32:37 $").

:- export
        http_server/1,
        http_server/2.



:- use_module(http_grammar).


/*
http_server(+Port)
    - creation of a socket, bind it to current Host and Port and listen
    - loop
*/

http_server(Port):-
        http_server(Port, 1).

http_server(Port, Pending):-
	setval(port, Port),
	socket(internet, stream, Soc),
	bind(Soc, _/Port),
	listen(Soc, Pending),
	loop(Soc).


/*
loop:
    - accept a connection on the socket
    - reception of a request
    - decoding of the request (method + url + http param init)
    - call of the server function
    - encoding of the response (depending on server function)
    - send the response on the socket
*/
loop(Soc):-
	accept(Soc, _, S),
        (block(process(S), _Tag, true) ->
            true
        ;
            true
        ),
	close(S),!,
	loop(Soc).
loop(Soc):-
	loop(Soc).


process(S):-
	request_recp(S, Method, URL, ReqHttpParams, ObjectBody),
	call(http_method(Method, URL, ObjectBody, 
	  Output, Status, RespHttpParams), http_method),
	analyse_para(ReqHttpParams, RespHttpParams, HttpParams),
	respons_enco(Output, Status, HttpParams, Response),
	respons_send(S, Response).
        

/*
The request reception reads from the socket stream a full request sent by
the client. Only http full requests are recognised (http document p. 15).

The request is returned as the Method, the URL and a set of parameters
defined in the header, plus the object body

The header is constituted of lindes separated by \r \n.
The header is separated from the object body by a \r \n.
The length of the object body is a parameter in the header
*/
request_recp(S, Method, Url, HttpParams, ObjectBody):-	
	read_parse_Header(S, Method, Url, HttpParams),
	(member(contentLength(BL), HttpParams) -> 
	    read_Object(S, BL, ObjectBody)
	; 
	    true 
	).


read_parse_Header(Stream, Method, Url, HttpParams):-
	read_SL(Stream, SL),
	parse_SL(SL, Method, Url),
	read_Params(Stream, Params),
	parse_Params(Params, HttpParams). 

/* 
status line
*/
read_SL(S, Line):-
	read_string(S, "\r\n", _, Line),
	read_string(S, "\r\n", L, _), L==0.

parse_SL(Line, Method, Url):-
	open(Line, string, s),
	read_string(s, " ", _, Method),
	read_string(s, " ", _, Url),
	close(s).

/*
general header*
request header*
object header*
*/
read_Params(S, List):-
	read_Params(S, [], List).

read_Params(S, L0, L):-
	read_string(S, "\r\n", Length, Elem),
	Length \== 0, !,
	read_string(S, "\r\n", LLength, _),
	LLength == 0,
	append(L0, [Elem], L1),
	read_Params(S, L1, L).
read_Params(S, L, L):-
	read_string(S, "\r\n", Length, _),
	Length == 0.


/*
parsing using the DCG grammar
*/

parse_Params([], []).
parse_Params([H|T], [P|TT]):-
	append_strings(H, "\n", HH),
	open(HH, string, s),
	token_to_list(s, L),
	close(s),
	phrase(header(P), L, _), !,
	parse_Params(T, TT).


/* 
read the object according to the object length that is contained
in the header.

*/
read_Object(S, BL, ObjectBody):-
	read_string(S, "", BL, ObjectBody).


/*
analyse of the Url
*/
analyse_Url(Url, Path):-
	open(Url, string, s),
	read_string(s, "//", _, _),
	read_string(s, "/", _, _),
	read_string(s, "", Path, _),
	close(s).


	

/* 
rpc call:
executes the method on the object and return:
- the output of the method (possibly empty)
- a status code for the response status line
- a list of http parameters (in particular the length of the object body).
	

http_rpcInterface(Method, Url, ObjectBody, Output, 200, [contentLength(CL)]):-
	concat_string(["Vous avez demande la methode ", 
	     Method, "sur l'objet ", Url], Output),
	string_length(Output, CL).
	
*/
/*
http paramter analysis:
compatibility between response and request parameters.
constitution of the list of parameters for the response
*/

analyse_para(_, HttpParams, HttpParams).


/*
response encoding:
- response status
- response header
- object body
*/

respons_enco(Output, Status, HttpParams, Response):-
	resp_SL(Status, SL),
	resp_Header(HttpParams, Header),
	concat_string([SL, Header, "\n" ], H),
	concat_strings(H, Output, Response).

resp_SL(Status, SL):-
	integer_atom(Status, S), atom_string(S, SC),
	substring(SC, 1, 1, RP),
	concat_string(["HTTP/1.0 ", SC, " ", RP, "\n"], SL).

resp_Header(HttpParams, Header):-
	resp_Header(HttpParams, "", Header).

resp_Header([], S, S).
resp_Header([H|T], S0, S):-
	phrase(header(H), L, _),
	open(_, string, s),
	list_to_token(L, s),
	current_stream(S1, string, s), close(s),
	concat_string([S0, S1, "\n"], S2),
	resp_Header(T, S2, S).

/* 
response sending
*/

respons_send(Stream, String):-
	open(String, string, StreamString),
	sub_respons_send(Stream, StreamString),
	close(StreamString).


sub_respons_send(Stream, StreamString):-
	read_string(StreamString, "", 250, String),!,
	write(Stream, String), flush(Stream),
        sub_respons_send(Stream, StreamString).
sub_respons_send(_, _).


%----------------------------------------------------------------------

:- comment(http_server/1, [ template:"http_server(+Port)",
    summary:"Start an http server",
    args:["Method":"An integer port number"],
    desc:html("
    The server does: 
    <UL>
    <LI> creation of a socket, bind it to current Host and given Port and listen 
    <LI> accept a connection on the socket 
    <LI> reception of a request 
    <LI> decoding of the request (method + url + http param init) 
    <LI> call the predicate http_method in module http_method 
    <LI> encoding of the response (depending on server function) 
    <LI> send the response on the socket 
    </UL>
    NOTE:  The predicate http_server/1 requires that a module http_method
    is defined that contains a predicate http_method/6.  This predicate is
    used by the programmer to customize the server.  For instance the
    method GET can be simply implemented.  The programmer can define its
    own methods. 
    <P>
    A simple example of server is the implementation of the method
    GET.  A module is created that contains the predicate
    http_method/6 that implements the method GET:  a read on the file
    identified by its URL.  The file is returned if it is found,
    otherwise an error parameter is returned. 
    <P>
    This simple program can be used to test HTML pages.  Viewers such
    as Netscape provide a view code option that signalizes syntax
    errors in the HTML code.  This simple program can be used as a
    light weight testing tool, possibly launched from the directory
    where the HTML page resides. 
    "),
    eg:"
    [eclipse 1]: [user].
     
    /********************************************************************
     *  test (server)
     *******************************************************************/

    :- module(http_method).

    :- set_error_handler(170, fail/0).
    :- set_error_handler(171, fail/0).

    /* 
    http_method(+Method, +Url, +ObjectBody, -Output, -StatusCode, -Parameter)
    executes the method on the object and returns:
    - the output of the method (possibly empty)
    - a status code for the response status line
    - a list of http parameters (in particular the length of the object body).

    */


    http_method(\"GET\", Url, _, Contents, 200, [contentLength(CL)]):-
	    append_strings(\"/\", FileName, Url),
	    getContents(FileName, Contents), !,
	    string_length(Contents, CL).
    http_method(\"GET\", _, _, \"\", 404, []).
	    

    getContents(Url, Contents):-
	    open(Url, read, s),
	    read_string(s, \"\", _, Contents),
	    close(s).

    ^D

    yes.

    [eclipse 2]: use_module(http).
    http_grammar.pl compiled traceable 25048 bytes in 0.27 seconds
    http_client.pl compiled traceable 6052 bytes in 0.28 seconds
    http_server.pl compiled traceable 5564 bytes in 0.03 seconds
    http.pl    compiled traceable 0 bytes in 0.35 seconds

    yes.
    [eclipse 3]: use_module(http_method).

    yes.
    [eclipse 4]: http_server(8000).
    "]).

:- comment(http_server/2, [ template:"http_server(+Port,+Pending)",
    summary:"Start an http server with allows the specified number of pending connections",
    args:["Method":"An integer port number",
          "Pending":"The number of simultaneous connections to queue up before rejecting"],
    desc:html("<CODE>http_server/1</CODE> corresponds to a pending queue size of 1.")]).
