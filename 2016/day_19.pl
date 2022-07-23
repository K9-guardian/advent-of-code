:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

% Josephus problem (https://en.wikipedia.org/wiki/Josephus_problem)
p1(S) :-
    phrase_from_file(integer(N), 'input/d19.txt'),
    2^A + L #= N, [L, A] ins 0..N,
    once(labeling([max(A)], [A, L])),
    S #= 2 * L + 1.

% TODO: Part 2.