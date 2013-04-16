% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: http_client.pl,v 1.9 2001/09/13 17:48:56 js10 Exp $
% ----------------------------------------------------------------------

/*
    RPC using HTTP/1.0 (request status line) and
    MIME-Version:1.0 (request general header)

    CLIENT

    http document used is HTTP/1.0 from the Network Working Group
    - internet draft - expiring in june, 19 1995.
*/


:- module(http_client).

:- comment(summary, "HTTP client library").
:- comment(author, "Ph. Bonnet, S. Bressan and M. Meier, ECRC Munich").
:- comment(copyright, "Imperial College London and ICL").
:- comment(date, "$Date: 2001/09/13 17:48:56 $").

:- comment(http_client/7, [
    template:"http_client(+Method, +Uri, +ObjectBody, +HttpParams, -RespError, -RespParam, -RespObjectBody)",
    summary:"Used to access HTML pages, given their URI (the method GET is applied)",
    args:["Method":"A string","Uri":"A string","ObjectBody":"A string",
    	"HttpParams":"A list of terms as defined in the DCG grammar",
	"RespError":"Outputs a term error(ErrorCode, ErrorPhrase),
	where ErrorCode is he error code contained in the response
	and ErrorPhrase is the error phrase contained in the response",
	"RespParam":"Outputs a list of terms as defined in the DCG grammar",
	"RespObjectBody":"Outputs the object body of the response"],
    eg:"
    [eclipse 1]: use_module(http).
    http_grammar.pl compiled traceable 25048 bytes in 0.38 seconds
    http_client.pl compiled traceable 5916 bytes in 0.47 seconds
    http_server.pl compiled traceable 5304 bytes in 0.07 seconds
    http.pl    compiled traceable 0 bytes in 0.57 seconds

    yes.
    [eclipse 2]:  http_client(\"GET\", \"http://www.ecrc.de/staff/\", \"\", [],
	    Status, Param, Resp).

    Status = error(200, \"Document follows \")
    Param = [date, server, contentType(mt(text, html))]

    Resp = \"<HTML>...</HTML>\"

    yes.
    "]).





:- export
        http_compile/1,
        http_compile/2,
        http_open/2,
        http_client/7.

:- tool(http_compile/1).


:- use_module(http_grammar).


/*
http_client(+Method, +Uri, +ObjectBody, +HttpParams, 
	    -RespError, -RespParams, -RespObjectBody): 
Metod and Uri and ObjectBody and String are strings
HttpParams is a list of terms defined in the DCG (http_grammar.pl)
Error is a term error(ErrorCode, ErrorPhrase)
   ErrorCode: the error code contained in the response
   ErrorCode: the error phrase contained in the response
RespParams is a list of terms defined in DCG (http_grammar.pl)
RespObjectBody is a string containing the object body of the response

The client does:
    - encoding of a request
    - sending of a request
    - reception of the response
    - decoding of the response (nothing)

HttpParams are the parameteres used to constitute the header.

*/

http_client(Method, Uri, ObjectBody, HttpParams, 
	    RespError, RespParams, RespObjectBody):-
    request_enco(Method, Uri, ObjectBody, HttpParams, HostName, Request),
    request_send(HostName, Request, Soc),
    respons_recp(Soc, RespError, RespParams, RespObjectBody),
    close(Soc),!.

/*
The request encoding composes a full request.
The host name and the port are obtained from the uri
http document p.15

*/
request_enco(Method, Uri, ObjectBody, HttpParams, HostName, Request):-   
    hostname(Uri, HostName, PartialURL),
    status_line(Method, PartialURL , SL),
    resp_header(HttpParams, ObjectBody, H),
    concat_strings(SL, H, St1),
    concat_strings(St1, "\r\n", St2),
    concat_strings(St2, ObjectBody, Request).

/* 
url decoding for http
*/
hostname(Uri, HostName, PartialURL):-
    append_strings("http://", S, Uri),
    open(S, string, St),
    read_string(St, "/", _, HostName), close(St),
    append_strings(HostName, PartialURL, S), !.

/*
Constitution of The header starting from the http parameters 
and the length of the object body
*/

status_line(Method, URL, SL):-
    concat_strings(Method, " ", St1),
    concat_strings(St1, URL, St2),
    concat_strings(St2, " ", St3),
    concat_strings(St3, "HTTP/1.0\r\n", SL).

resp_header(HttpParams, ObjectBody, H):-
    params_format(HttpParams, P),
    string_length(ObjectBody, L),
    (L == 0 ->
	H = P
    ; 
	params_format([contentLength(L)], CL),
	% could become a pb concat_strings is better
	concat_string([P, CL], H)
    ).

params_format(HttpParams, P):-
	params_format(HttpParams, "", P).

params_format([], S, S).
params_format([H|T], S0, S):-
    phrase(header(H), L, _),
    open(_, string, St),
    list_to_token(L, St),
    current_stream(S1, string, St), close(St),
    concat_string([S0, S1, "\r\n"], S2),
    params_format(T, S2, S).	

/* 
sending of a request:
- a socket is created  and connected to hostName
- the encoded request is sent
*/

request_send(HostName, Request, Soc):-
    host_and_port(HostName, HN, P),
    socket(internet, stream, Soc),
    connect(Soc, HN / P), 
    concat_strings(Request, "\r\n", R),
    write(Soc, R),
    flush(Soc).
    

host_and_port(HostName, HH, 80):-
    open(HostName, string, St),
    read_string(St, ":", L, H),
    close(St),
    string_length(HostName, L),!,
    atom_string(HH, H).
host_and_port(HostName, HHH, PPP):-
    open(HostName, string, St),
    read_string(St, ":", _, H),
    close(St),
    concat_strings(H, ":", HH),
    append_strings(HH, P, HostName),
    atom_string(HHH, H),
    atom_string(PP, P), integer_atom(PPP, PP), !.

    

/*
reception of a response:
- read a message until a CRLF is found -> response status line
    % status error management
- analyse the response header -> length, type, encoding of the object body
- read the object body
*/

respons_recp(S, Error, HttpParams, ObjectBody):-
    read_Error(S, Error),!,
    (Error = error(200,_) ->
	read_ParamsClient(S, Params),
        parse_Params(Params, HttpParams),
        read_objectBody(S, HttpParams, ObjectBody)
    ;
	true
    ).

read_Error(S, error(ErrorCode, ErrorPhrase)):-
	read_string(S, end_of_line, _, SL),
	open(SL, string, St),
	token_to_list(St, L),
	phrase(status(_, ErrorCode, ErrorPhrase), L, _),
	close(St).

/*
general header*
request header*
object header*
*/
read_ParamsClient(S, List):-
	% \n instead of \r\n as in the http document
	read_string(S, end_of_line, _Length, Elem0),
	( Elem0 \= "", Elem0 \= "\r" ->
	    append_strings(Elem0, "\n", Elem),
	    List = [Elem|Tail],
	    read_ParamsClient(S, Tail)
	;
	    List = []
	).


/*
parsing using the DCG grammar
*/
parse_Params([], []).
parse_Params([H|T], [P|TT]):-
	open(H, string, St),
	token_to_list(St, L),
	close(St),
	phrase(header(P), L, _), !,
	parse_Params(T, TT).

/* 
read object body according to the content length in the http params
*/
read_objectBody(S, HttpParams, ObjectBody):-
	member(contentLength(L), HttpParams),!,
	read_string(S, "", L, ObjectBody).
read_objectBody(S, HttpParams, ObjectBody):-
	member(contentType(_), HttpParams),!,
	read_string(S, "", _, ObjectBody).
read_objectBody(_, _, "").

/*
No response decoding.
*/


:- comment(http_open/2, [
    template:"http_open(+Url, -Stream)",
    summary:"Download a web document given its URL and open a Stream to read it",
    args:["Url":"A string","Stream":"A variable or atom"],
    desc:html("This utility downloads a web document (given its URL) into a
    string stream and returns that string stream's identifier for reading."),
    eg:"
    [eclipse 1]: lib(http_client).
    yes.
    [eclipse 2]: http_open(\"http://icparc.ic.ac.uk/index.html\",S),
                 read_string(S, end_of_line, _, L).
    S = 19
    L = \"<!DOCTYPE HTML PUBLIC \\\"-//W3C//DTD HTML 3.2//EN\\\">\"
    yes.
    "]).

http_open(URL, Stream) :-
	http_client("GET", URL, "", [], Error, _Params, Response),
	( Error = error(200, _OK) ->
	    open(Response, string, Stream)
	;
	    fail
	).


:- comment(http_compile/1, [
    template:"http_compile(+Url)",
    summary:"Compile an ECLiPSe source file, given its URL",
    args:["Url":"A string"],
    desc:html("This utility downloads an eclipse source file (given its URL)
    and compiles it. Note that this represents a security risk: the downloaded
    code may contain Eclipse commands that are executed on your computer.
    Make sure you trust the code that you download!"),
    eg:"
    [eclipse 1]: lib(http_client).
    yes.
    [eclipse 2]: http_compile(\"http://icparc.ic.ac.uk/eclipse/examples/sendmore.pl\").
    yes.
    [eclipse 8]: sendmore1(X).
    X = [9, 5, 6, 7, 1, 0, 8, 2]     More? (;) 
    no (more) solution.
    "]).

:- tool(http_compile/1, http_compile/2).
http_compile(URL, Module) :-
	http_open(URL, Stream),
	compile_stream(Stream)@Module,
	close(Stream).

