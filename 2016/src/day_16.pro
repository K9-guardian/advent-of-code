:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).
:- use_module(library(edcg)).
:- use_module(library(lazy_lists)).

% Precompute input and inverted input.

:- table input/1, input_length/1, abdb_length/1, input_inverted/1, input_parity/1.

input(S) :- phrase_from_file(string(S), 'input/d16.txt').

input_length(L) :- input(S), length(S, L).

abdb_length(L) :- input_length(L0), L #= L0 * 2 + 2.

input_inverted(I) :- input(S), phrase(inverted_(S), I).

input_parity(P) :- input(S), input_inverted(I), append(S, I, SI), foldl(xnor, SI, '1', P).

bit_inverse('0') --> "1".
bit_inverse('1') --> "0".

inverted_([]) --> [].
inverted_([B|Bs]) --> inverted_(Bs), bit_inverse(B).

dragon("0").
dragon(Ds) :-
    dragon(Ds0),
    phrase((Ds0, "0", inverted_(Ds0)), Ds).

dragon_segment_(D0, D1) --> { input(S), input_inverted(I) }, S, [D0], I, [D1].

bitstring_ones([], 0).
bitstring_ones(['0'|Bs], N) :- bitstring_ones(Bs, N).
bitstring_ones(['1'|Bs], N) :- N #= 1 + N1, bitstring_ones(Bs, N1).

xnor('0', '0', '1').
xnor('1', '0', '0').
xnor('0', '1', '0').
xnor('1', '1', '1').

cycle(A-B, B-A, A).

join([[I|Is]|Iss]-[], [Is|Iss]-[], I).
join([[]|Is]-[D|Ds], Is-Ds, D).
join([[I|Is]|Iss]-Ds, [Is|Iss]-Ds, I).

dragon_total(Ds, T) :-
    lazy_list(cycle, input(~)-input_inverted(~), Is),
    lazy_list(join, Is-Ds, T).

edcg:pass_info(chunk_size).

edcg:acc_info(total_curve, N, _-Ds0, F-Ds, n_list_split(N, Ds0, F, Ds)).

edcg:acc_info(chunk_size_remaining, N, _, N, true). % This design pattern is so weird
edcg:acc_info(joiner_curve, N, _-Js0, F-Js, n_list_split(N, Js0, F, Js)).
edcg:acc_info(leftover_parity, P, _, P, true).

edcg:pred_info(p1_segments_checksum_, 1, [chunk_size, total_curve, dcg]).

edcg:pred_info(joiner_parity_, 2, [joiner_curve]).
edcg:pred_info(leftover_parity_, 3, [joiner_curve]).
edcg:pred_info(p2_segments_checksum_,
               1,
               [chunk_size,
                chunk_size_remaining,
                joiner_curve,
                leftover_parity,
                dcg]).

p1_segments_checksum_(0) -->> [].
p1_segments_checksum_(s(N)) -->>
    Cs/chunk_size,
    [Cs]:total_curve,
    (T-_)/total_curve,
    { foldl(xnor, T, '1', V) },
    [V],
    p1_segments_checksum_(N).

joiner_parity_(N0, P) -->>
    { N #= N0 * 2 },
    [N]:joiner_curve,
    (Js-_)/joiner_curve,
    { foldl(xnor, Js, '1', P) }.

% This predicate doesn't have to be in the edcg but it makes it clean.
input_parity_(N, P) -->>
    { input_parity(Par), % Parity from folding over our input and inverted input.
      n_list_repeated(N, [Par], Is),
      foldl(xnor, Is, '1', P)
    }.

leftover_parity_(R, P0, P1) -->>
    [2]:joiner_curve,
    ([J0, J1]-_)/joiner_curve,
    { phrase(dragon_segment_(J0, J1), S),
      n_list_split(R, S, H, T),
      foldl(xnor, H, '1', P0),
      foldl(xnor, T, '1', P1)
    }.

p2_segments_checksum_(0) -->> [].
p2_segments_checksum_(s(N)) -->>
    Cs/chunk_size,
    Csr0/chunk_size_remaining,
    Lp0/leftover_parity,
    { abdb_length(L),
      divmod(Csr0, L, Quot, Rem),
      Csr #= Cs - (36 - Rem)
    },
    [Csr]:chunk_size_remaining,
    joiner_parity_(Quot, Jp),
    input_parity_(Quot, Ip),
    leftover_parity_(Rem, Fp, Lp),
    { foldl(xnor, [Lp0, Jp, Ip, Fp], '1', V) },
    [Lp]:leftover_parity,
    [V],
    p2_segments_checksum_(N).

p1(S) :-
    L is ceiling(272 / 18),
    length(Cs, L), append(Cs, _, Ds),
    once(dragon(Ds)),
    dragon_total(Ds, T),
    2^N * R #= 272, P #= 2^N, N in 0..272,
    once(labeling([max(N)], [N, R])), peano_natural(Rp, R),
    phrase(p1_segments_checksum_(Rp, P, []-T, _), S).

p2(S) :-
    L is ceiling(35_651_584 / 18),
    length(Cs, L), append(Cs, _, Ds),
    once(dragon(Ds)),
    2^N * R #= 35_651_584, P #= 2^N, N in 0..35_651_584,
    once(labeling([max(N)], [N, R])), peano_natural(Rp, R),
    phrase(p2_segments_checksum_(Rp, P, P, _, []-Ds, _, '1', _), S).
