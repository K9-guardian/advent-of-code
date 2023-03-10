:- use_foreign_library(share/hash_helper).
:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

p1(S) :-
    phrase_from_file(string(Salt), 'input/d14.txt'),
    once(findnsols(64, K, salt_stretch_key(Salt, 0, K), Ks)),
    nth1(64, Ks, S).

p2(S) :-
    phrase_from_file(string(Salt), 'input/d14.txt'),
    once(findnsols(64, K, salt_stretch_key(Salt, 2016, K), Ks)),
    nth1(64, Ks, S).
