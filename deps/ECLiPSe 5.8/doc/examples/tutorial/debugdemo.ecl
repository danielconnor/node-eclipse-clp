:- local variable(pid).

:- [mapcolour].

exec_mapdisplay(Pid, Port) :-
        (get_flag(hostarch, "i386_nt") ->
             exec([wish83, "mapdebugdemo.tcl", "--", "-p", Port], [], Pid)
        ;
             exec([wish, "mapdebugdemo.tcl", "--", "-p", Port], [], Pid)
        ),
        setval(pid, Pid).



disconnect_handler :-
        (get_flag(hostarch, "i386_nt") -> 
             true ; getval(pid, Pid), wait(Pid, _)
        ).

post_attach(S) :-
        set_event_handler(S, disconnect_handler/0).

:- remote_connect_setup(localhost/Port, Peer, Sock),
   exec_mapdisplay(Pid, Port),
   (remote_connect_accept(Peer, Sock, 10, post_attach(Peer), "", _) ->
        true ; close(Sock)
   ).

clear :-
        get_map_data(4).

colour :-
        colouring1(prolog, input_order, indomain, 4, _).


colourdelay :-
        colouring1(delay, input_order, indomain, 4, _).

