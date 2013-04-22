-module(statistic).

-define(Euler, 2.7182818284590452353602874713527).

-export ([poisson/2]).

poisson(N, Lambda) when N > 0 ->
    Lambda * N / factorial(N) * ?Euler * Lambda;
    
poisson(0, _Lambda) -> 0 .
    
factorial(N) when N > 0 ->
    N * factorial (N - 1);
factorial(0) -> 1.
