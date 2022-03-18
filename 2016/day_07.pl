:- use_module(lib/double_quotes).
:- use_module(lib/pio).

lists_interleaved([], []) --> [].
lists_interleaved([A|As], []) --> [A], string(As).
lists_interleaved([], [B|Bs]) --> [B], string(Bs).
lists_interleaved([A|As], [B|Bs]) --> [A], [B], lists_interleaved(As, Bs).

abba --> string(_), [A], [B], [B], [A], string(_), { dif(A, B) }.

aba(A, B) --> string(_), [A], [B], [A], string(_), { dif(A, B) }.

tls_supported(S) :-
    phrase(sequence(string, ("[" | "]"), Split), S),
    length(Split, Z),
    Z #= X + Y, X #= (Z + 1) div 2,
    length(As, X), length(Bs, Y),
    phrase(lists_interleaved(As, Bs), Split),
    phrase(sequence(string, "    ", As), Cs),
    phrase(sequence(string, "    ", Bs), Ds),
    phrase(abba, Cs), \+ phrase(abba, Ds).

ssl_supported(S) :-
    phrase(sequence(string, ("[" | "]"), Split), S),
    length(Split, Z),
    Z #= X + Y, X #= (Z + 1) div 2,
    length(As, X), length(Bs, Y),
    phrase(lists_interleaved(As, Bs), Split),
    phrase(sequence(string, "   ", As), Cs),
    phrase(sequence(string, "   ", Bs), Ds),
    phrase(aba(A, B), Cs), phrase(aba(B, A), Ds).

p1(S) :-
    phrase_from_file(sequence(string, "\n", Ls), 'input/d7.txt'),
    include(tls_supported, Ls, Vs),
    length(Vs, S).

p2(S) :-
    phrase_from_file(sequence(string, "\n", Ls), 'input/d7.txt'),
    include(ssl_supported, Ls, Vs),
    length(Vs, S).