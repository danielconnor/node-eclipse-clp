% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: http.pl,v 1.5 2001/09/13 17:48:56 js10 Exp $
% ----------------------------------------------------------------------

:- module(http).

:- comment(summary, "HTTP library").
:- comment(author, "Ph. Bonnet, S. Bressan and M. Meier, ECRC Munich").
:- comment(copyright, "Imperial College London and ICL").
:- comment(date, "$Date: 2001/09/13 17:48:56 $").
:- comment(desc, html("
    The HTTP library contains an extensible server and a client for the
    Hyper Text Transfer Protocol.  The library is entirely written in ECLiPSe. 
    <P>
    Typical use of the client is for building WWW \"Worms\", WWW
    \"Robots\" or customized WWW browsers.  Typical use of the server is
    for building customized servers, e.g.  dynamic generation of HTML
    pages.  The server and the client can typically be used together
    to build proxy servers. 
    <P>
    Limitations and Bugs: 
    <UL>
    <LI> The current version of the server is sequential. 
    <LI> The MIME and HTTP grammar is not complete and may fail
	parsing some sentences generated by existing browsers and servers. 
    </UL>
    The library consists of two sub-modules, http_client and http_server,
    which can also be loaded separately.
    <P>
    The structure of the HTTP messages is precisely described in the
    specification document (http://www.w3.org/pub/WWW/Protocols/).  An
    augmented BNF is provided for each component of the header.  We
    have used the DCG (Definite Clause Grammar) mechanism of ECLiPSe
    to encode the grammar, that we use for both parsing (from HTTP
    messages into Prolog terms) and pretty printing (from prolog terms
    into HTTP messages). 
    <P>
    This DCG grammar may have to be modified with the evolutions of
    the HTTP protocol (standard modification and available client or
    server implementations). 
    ")).

:- reexport http_client.
:- reexport http_server.
