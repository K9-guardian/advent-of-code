:- use_foreign_library(share/hash_helper).
:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

p1(S) :-
    phrase_from_file(string(ID), 'input/d5.txt'),
    once(findnsols(8, C, data_prefix_suffix(ID, "00000", [C|_]), S)).

data_passwd(ID, Passwd) :-
    repeat,
    data_prefix_suffix(ID, "00000", [N0, C|_]),
    (   ground(Passwd)
    ->  !
    ;   N = succ $ atom_number $ N0,
        var(arg(N, Passwd, ~)),
        nb_setarg(N, Passwd, C),
        fail
    ).

p2(S) :-
    phrase_from_file(string(ID), 'input/d5.txt'),
    functor(Passwd, p, 8),
    data_passwd(ID, Passwd),
    Passwd =.. [_|S].
