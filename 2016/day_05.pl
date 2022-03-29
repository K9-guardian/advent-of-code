:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).
:- use_module(library(md5)).
:- use_module(library(pairs)).

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

% I can't improve this.
room_tail_found_(ID, X, Found, Out) :-
    (   dict_size(Found, 8)
    ->  pairs_values(dict_pairs(Found, _, ~), Out)
    ;   (   input_keys(atom_concat(ID, X, ~), N-C),
            call_dcg((atom_number, in), N, 0..8),
            \+ get_dict(N, Found, _)
        ->  room_tail_found_(ID, succ $ X, Found.put(N, C), Out)
        ;   room_tail_found_(ID, succ $ X, Found, Out)
        )
    ).

p1(S) :-
    phrase_from_file(string(ID0), 'input/d5.txt'),
    atom_chars(ID, ID0),
    findnsols(8, C, (length(_, X), call_dcg((atom_concat(ID), input_key), X, C)), Cs),
    S = Cs.

p2(S) :-
    phrase_from_file(string(ID0), 'input/d5.txt'),
    atom_chars(ID, ID0),
    room_tail_found_(ID, 0, found{}, S).