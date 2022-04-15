:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).
:- use_module(library(md5)).

:- table salt/1.

salt(S) :- phrase_from_file(string(S0), 'input/d14.txt'), atom_chars(S, S0).

three_in_row(A) --> string(_), [A, A, A], string(_).
five_in_row(A) --> string(_), [A, A, A, A, A], string(_).

n_index_hashes(N, I, Hs) :-
    (   N = 1000
    ->  Hs = []
    ;   atom_concat(salt(~), I, SI),
        md5_hash(SI, H0, []),
        atom_chars(H0, H),
        Hs = [I-H|Hs1],
        n_index_hashes(succ $ N, succ $ I, Hs1)
    ).

n_hash0_hash(N, H0, H) :-
    (   N = 2016
    ->  H = H0
    ;   md5_hash(H0, H1, []),
        n_hash0_hash(succ $ N, H1, H)
    ).

p2_n_index_hashes(N, I, Hs) :-
    (   N = 1000
    ->  Hs = []
    ;   atom_concat(salt(~), I, SI),
        n_hash0_hash(0, md5_hash(SI, ~, []), H0),
        atom_chars(H0, H),
        Hs = [I-H|Hs1],
        n_index_hashes(succ $ N, succ $ I, Hs1)
    ).

hashes0_hashes1_keys([], _, []).
hashes0_hashes1_keys([I-H|Hs0], Hs1, Ks) :-
    (   once(phrase(three_in_row(A), H)),
        append(Hs0, Hs1, Hs), n_list_split(1000, Hs, HsP, _),
        include({A}/[_-H]>>phrase(five_in_row(A), H), HsP, [_|_])
    ->  Ks = [I-H|Ks1]
    ;   Ks = Ks1
    ),
    hashes0_hashes1_keys(Hs0, Hs1, Ks1).

part_index_keys0_keys(P, I0) -->
    { ( P = 1 -> G = n_index_hashes ; G = p2_n_index_hashes ),
      I #= I0 + 1000,
      call(G, 0, I0, Hs0),
      call(G, 0, I, Hs1),
      hashes0_hashes1_keys(Hs0, Hs1, Ks)
    },
    Ks.

part_index_keys0_keys(P, I0) -->
    { ( P = 1 -> G = n_index_hashes ; G = p2_n_index_hashes ),
      I #= I0 + 1000,
      call(G, 0, I0, Hs0),
      call(G, 0, I, Hs1),
      hashes0_hashes1_keys(Hs0, Hs1, Ks)
    },
    Ks,
    part_index_keys0_keys(P, I).

p1(S) :-
    L #>= 64,
    length(Ks, L),
    phrase(part_index_keys0_keys(1, 0), Ks),
    nth1(64, Ks, S).

p2(S) :-
    L #>= 64,
    length(Ks, L),
    phrase(part_index_keys0_keys(2, 0), Ks),
    nth1(64, Ks, S).