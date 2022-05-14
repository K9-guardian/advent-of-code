:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

bit_inverse('0') --> "1".
bit_inverse('1') --> "0".

inverse([]) --> [].
inverse([B|Bs]) --> inverse(Bs), bit_inverse(B).

dragon(Bs) :- phrase_from_file(string(Bs), 'input/d16.txt').

dragon(Bs) :-
    dragon(Bs0),
    phrase((string(Bs0), "0", inverse(Bs0)), Bs).

string_checksum(S, C) :-
    (length $ S) mod 2 #= R,
    if_(R = 1,
        C = S,
        (phrase(string_checksum_(S), S1),
         string_checksum(S1, C))).

string_checksum_([]) --> [].
string_checksum_([A,B|Ls]) --> { if_(A = B, C = '1', C = '0') }, [C], string_checksum_(Ls).

p1(S) :-
    length(Bs, 272),
    append(Bs, _, Ls),
    once(dragon(Ls)),
    string_checksum(Bs, C),
    S = C.

p2(S) :-
    length(Bs, 35651584),
    append(Bs, _, Ls),
    once(dragon(Ls)),
    string_checksum(Bs, C),
    S = C.