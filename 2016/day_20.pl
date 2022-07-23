:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

range(X..Y) --> integer(X), ['-'], integer(Y).

p1(S) :-
    phrase_from_file(sequence(range, "\n", Rs), 'input/d20.txt'),
    IP in 0..4294967295,
    maplist({IP}/[R]>>(#\ IP in R), Rs),
    once(labeling([min(IP)], [IP])),
    S = IP.

p2(S) :-
    phrase_from_file(sequence(range, "\n", Rs), 'input/d20.txt'),
    IP in 0..4294967295,
    maplist({IP}/[R]>>(#\ IP in R), Rs),
    fd_size(IP, S).