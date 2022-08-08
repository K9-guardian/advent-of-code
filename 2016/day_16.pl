:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).
:- use_module(library(lazy_lists)).

:- set_prolog_flag(optimise, true).

% Precompute input and inverted input.
% Prolog has variable declarations lmao

:- table input/1.
input(S) :- phrase_from_file(string(S), 'input/d16.txt').

:- table input_length/1.
input_length(L) :- input(S), length(S, L).

:- table input_inverted/1.
input_inverted(I) :- input(S), phrase(inverted_(S), I).

:- table input_parity/1.
input_parity(P) :- input(S), input_inverted(I), append(S, I, SI), foldl(xnor, SI, '1', P).

bit_inverse('0') --> "1".
bit_inverse('1') --> "0".

inverted_([]) --> [].
inverted_([B|Bs]) --> inverted_(Bs), bit_inverse(B).

dragon("0").
dragon(Ds) :-
    dragon(Ds0),
    phrase((Ds0, "0", inverted_(Ds0)), Ds).

dragon_segment(D0, D1) --> { input(S), input_inverted(I) }, S, [D0], I, [D1].

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

segments_partition_total_checksum(0, _, _) --> !, [].
segments_partition_total_checksum(N0, P, T0) -->
    { n_list_split(P, T0, F, T),
      foldl(xnor, F, '1', V),
      N #= N0 - 1 },
    [V],
    segments_partition_total_checksum(N, P, T).

segments_total_partition_joiners_leftover_checksum(0, _, _, _, _) --> !, [].
segments_total_partition_joiners_leftover_checksum(N0, P, P0, Js0, L0) -->
    { divmod(P0, 36, Q, R),
      Q2 #= Q * 2,

      n_list_split(Q2, Js0, Js1, [J0, J1|Js]), % Joiner stream
      foldl(xnor, Js1, '1', JsP),

      input_parity(Par), % Inputs
      n_list_repeated(Q, [Par], Is),
      foldl(xnor, Is, '1', IsP),

      phrase(dragon_segment(J0, J1), Ds), % Leftover
      n_list_split(R, Ds, Fs, Ss),
      foldl(xnor, Fs, '1', FsP),

      foldl(xnor, [L0, JsP, IsP, FsP], '1', V), % Combine everything
      foldl(xnor, Ss, '1', L),
      N #= N0 - 1,
      P1 #= P - (36 - R) },
    [V],
    segments_total_partition_joiners_leftover_checksum(N, P, P1, Js, L).

p1(S) :-
    L is ceiling(272 / 18),
    length(Cs, L), append(Cs, _, Ds),
    once(dragon(Ds)),
    dragon_total(Ds, T),
    2^N * R #= 272, P #= 2^N, N in 0..272, once(labeling([max(N)], [N, R])),
    phrase(segments_partition_total_checksum(R, P, T), S).

p2(S) :-
    L is ceiling(35651584 / 18),
    length(Cs, L), append(Cs, _, Ds),
    once(dragon(Ds)),
    2^N * R #= 35651584, P #= 2^N, N in 0..35651584, once(labeling([max(N)], [N, R])),
    phrase(segments_total_partition_joiners_leftover_checksum(R, P, P, Ds, '1'), S).