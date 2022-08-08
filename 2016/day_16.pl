:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).
:- use_module(library(lazy_lists)).

:- set_prolog_flag(optimise, true).

% Precompute input and inverted input.

:- table input/1.

input(S) :- phrase_from_file(string(S), 'input/d16.txt').

:- table input_length/1.

input_length(L) :- input(S), length(S, L).

:- table input_inverted/1.

input_inverted(I) :- input(S), phrase(inverted_(S), I).

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

% Could generalize this but hard.
cycle(A-B, B-A, A).

join(inputs_dragon([[I|Is]|Iss], []), inputs_dragon([Is|Iss], []), I).
join(inputs_dragon([[]|Is], [D|Ds]), inputs_dragon(Is, Ds), D).
join(inputs_dragon([[I|Is]|Iss], Ds), inputs_dragon([Is|Iss], Ds), I).

dragon_total(Ds, S) :-
    lazy_list(cycle, input(~)-input_inverted(~), Is),
    lazy_list(join, inputs_dragon(Is, Ds), S).

xnor('0', '0', '1').
xnor('1', '0', '0').
xnor('0', '1', '0').
xnor('1', '1', '1').

segments_partition_total_checksum(0, _, _) --> !, [].
segments_partition_total_checksum(N0, P, T0) -->
    { n_list_split(P, T0, Ls, T),
      foldl(xnor, Ls, '1', V),
      N is N0 - 1 },
    [V],
    segments_partition_total_checksum(N, P, T).

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
    dragon_total(Ds, T),
    number_chars(S, T).
    % n_list_split(200, T, S, _).
    % 2^N * R #= 35651584, P #= 2^N, N in 0..35651584, once(labeling([max(N)], [N, R])),
    % phrase(segments_partition_total_checksum(R, P, T), S).