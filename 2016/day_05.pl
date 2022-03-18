:- use_module(lib/double_quotes).
:- use_module(library(md5)).
:- use_module(lib/pio).

% From library(dicts), won't load for some reason.
dict_size(Dict, KeyCount) :-
    must_be(dict,Dict),
    compound_name_arity(Dict,_,Arity),
    KeyCount is (Arity-1)//2.

input_key(A, C) :-
    md5_hash(A, H, []),
    atom_concat('00000', S, H),
    atom_chars(S, [C|_]).

input_keys(A, C0-C1) :-
    md5_hash(A, H, []),
    atom_concat('00000', S, H),
    atom_chars(S, [C0, C1|_]).

room_passwd(ID, P) :- room_tail_found_(ID, 0, found{}, P).

room_tail_found_(ID, X, Found, Out) :-
    (   dict_size(Found, 8)
    ->  dict_pairs(Found, _, Ps), pairs_values(Ps, Out)
    ;   atom_concat(ID, X, IDX),
        succ(X, Y),
        (   input_keys(IDX, N-C),
            member(N, "012345678"),
            call_dcg((char_code,in), N, 48..56),
            \+ get_dict(N, Found, _)
        ->  room_tail_found_(ID, Y, Found.put(N, C), Out)
        ;   room_tail_found_(ID, Y, Found, Out)
        )
    ).

p1(S) :-
    phrase_from_file(string(ID0), 'input/d5.txt'),
    atom_chars(ID, ID0),
    findnsols(8, C, (between(0, inf, X), atom_concat(ID, X, IDX), input_key(IDX, C)), Cs),
    S = Cs.

p2(S) :-
    phrase_from_file(string(ID0), 'input/d5.txt'),
    atom_chars(ID, ID0),
    room_passwd(ID, Cs),
    S = Cs.