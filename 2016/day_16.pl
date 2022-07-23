:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).
:- use_module(library(lazy_lists)).

bit_inverse('0') --> "1".
bit_inverse('1') --> "0".

inverse([]) --> [].
inverse([L|Ls]) --> inverse(Ls), bit_inverse(L).

joiner("0").
joiner(Cs) :-
    joiner(Cs0),
    phrase((string(Cs0), "0", inverse(Cs0)), Cs).

input_inversed_stream(S) :-
    phrase_from_file(string(Cs), 'input/d16.txt'),
    phrase(inverse(Cs), Inv),
    lazy_list([A-B, B-A, A]>>true, Cs-Inv, S).

joiner_merged(Js, Ms) :-
    phrase_from_file(string(Cs), 'input/d16.txt'),
    phrase(inverse(Cs), Inv),
    S = [Cs, Inv|S],
    phrase(joiner_stream_merged_(Js, S), Ms).

joiner_stream_merged_([], [A|_]) --> string(A).
joiner_stream_merged_([J|Js], [A|S]) -->
    string(A),
    [J],
    joiner_stream_merged_(Js, S).

xnor('0', '0', '1').
xnor('0', '1', '0').
xnor('1', '0', '0').
xnor('1', '1', '1').

p1(S) :-
    phrase_from_file(string(Cs), 'input/d16.txt'),
    length(Cs, L0), L #= 272 // (L0 + 1),
    length(Js, L), append(Js, _, Ls), once(joiner(Ls)),
    272 #= X * 2^N, N #>= 0, once(labeling([max(N)], [X])), M #= 2^N,
    joiner_merged(Js, Qs),
    foldl({M}/[_, Qs0-[O|Os], Qs-Os]>>
          (n_list_split(M, Qs0, Buf, Qs),
           foldl(xnor, Buf, '1', O)),
          numlist(1, X, ~),
          Qs-S,
          _-[]).

p2(S) :-
    phrase_from_file(string(Cs), 'input/d16.txt'),
    length(Cs, L0), L #= 35651584 // (L0 + 1),
    length(Js, L), append(Js, _, Ls), once(joiner(Ls)),
    35651584 #= X * 2^N, N #>= 0, once(labeling([max(N)], [X])), M #= 2^N,
    joiner_merged(Js, Qs),
    foldl({M}/[_, Qs0-[O|Os], Qs-Os]>>
          (n_list_split(M, Qs0, Buf, Qs),
           foldl(xnor, Buf, '1', O)),
          numlist(1, X, ~),
          Qs-S,
          _-[]).