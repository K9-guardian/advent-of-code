:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

abba --> string(_), [A], [B], [B], [A], string(_), { dif(A, B) }.

aba(A, B) --> string(_), [A], [B], [A], string(_), { dif(A, B) }.

tls_supported(S) :-
    phrase(sequence(string, ("[" | "]"), Split), S),
    length(Split, Z),
    Z #= X + Y, X #= (Z + 1) div 2,
    length(As, X), length(Bs, Y),
    lists_interleaved(As, Bs, Split),
    convlist(phrase(abba), As, [_|_]),
    convlist(phrase(abba), Bs, []).

ssl_supported(S) :-
    phrase(sequence(string, ("[" | "]"), Split), S),
    length(Split, Z),
    Z #= X + Y, X #= (Z + 1) div 2,
    length(As, X), length(Bs, Y),
    lists_interleaved(As, Bs, Split),
    phrase(sequence(string, "  ", As), S0),
    phrase(sequence(string, "  ", Bs), S1),
    phrase(aba(A, B), S0), phrase(aba(B, A), S1).

p1(S) :-
    phrase_from_file(sequence(string, "\n", Ls), 'input/d7.txt'),
    include(tls_supported, Ls, Vs),
    length(Vs, S).

p2(S) :-
    phrase_from_file(sequence(string, "\n", Ls), 'input/d7.txt'),
    include(ssl_supported, Ls, Vs),
    length(Vs, S).
