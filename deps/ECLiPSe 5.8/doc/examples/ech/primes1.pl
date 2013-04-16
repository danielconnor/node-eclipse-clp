:- lib(ech).

:- option(single_symmetric_simpagation, off).

:- constraints prime/1, primes/2.

primes(N) :- primes(1, N).

primes(N, N) <=> true.
primes(M, N) <=> M < N | M1 is M + 1, prime(M1), primes(M1, N).

prime(I) \ prime(J) <=> J mod I =:= 0 | true.
