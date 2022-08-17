:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).
:- use_module(library(dicts)).
:- use_module(library(md5)).
:- use_module(library(pairs)).

data_chars(A, C0-C1) :-
    md5_hash(A, H, []),
    atom_concat('00000', S, H),
    atom_chars(S, [C0, C1|_]).

n_data_passwd(_, _, Passwd) :- ground(Passwd), !.
n_data_passwd(N0, Data0-Salt0, Passwd) :-
    copy_term([Salt0], Data0, [Salt], Data),
    number_chars(N0, Salt0),
    (   data_chars(Data0, Pos0-Char),
        Pos = succ $ atom_number $ Pos0,
        var(arg(Pos, Passwd, ~))
    ->  setarg(Pos, Passwd, Char)
    ;   true
    ),
    n_data_passwd(succ $ N0, Data-Salt, Passwd).

p1(S) :-
    phrase_from_file(string(ID), 'input/d5.txt'),
    append(ID, Salt, Data),
    findnsols(8, C, (between(1, inf, X), number_chars(X, Salt), data_chars(Data, C-_)), Cs),
    S = Cs.

p2(S) :-
    phrase_from_file(string(ID), 'input/d5.txt'),
    append(ID, Salt, Data),
    functor(Passwd, p, 8),
    n_data_passwd(0, Data-Salt, Passwd),
    Passwd =.. [_|S].