:- lib(ech).

:- option(already_in_store, off).

:- constraints prime/1, primes/1.

primes(1) <=> true.
primes(N) <=> N > 1 | M is N - 1, prime(N), primes(M).

prime(I) \ prime(J) <=> J mod I =:= 0 | true.
