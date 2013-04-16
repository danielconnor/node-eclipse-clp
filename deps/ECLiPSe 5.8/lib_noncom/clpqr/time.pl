:- export time/1.
:- tool( time/1, time_body/2).
%
time_body( Goal, Module) :-
  statistics( runtime, _),
    call( Goal, Module),
  statistics( runtime, [_,T]), 
  print_time( T).
time_body( _, _) :- 
  statistics( runtime, [_,T]), 
  print_time( T),
  fail.

:- export session_time/1.
:- tool( session_time/1, session_time_body/2).
%
session_time_body( Goal, Module) :-
  statistics( session_time, T00),
  setval( start, T00),
    call( Goal, Module),
  statistics( session_time, T1),
  getval( start, T0),
  T is fix(1000*(T1-T0)),
  print_time( T),
  setval( start, T1).
session_time_body( _, _) :- 
  statistics( session_time, T1),
  getval( start, T0),
  T is fix(1000*(T1-T0)),
  print_time( T),
  fail.
      
%
% milliseconds
%
print_time( T) :-
  Seconds is float(T/1000),
  Hour is T // 3600000, 
  R0 is T-Hour*3600000,
  Min is R0 // 60000,
  R1 is R0-Min*60000,
  Sec is float(R1/1000),
  printf( '%%%%%% Timing %02d:%02d:%06.3f %9.3f', [Hour,Min,Sec,Seconds]), nl.
