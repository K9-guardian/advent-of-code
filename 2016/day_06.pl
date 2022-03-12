:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(library(yall)).

frequencies(Es, Freqs) :-
    foldl([E, Fs0, Fs]>>
          (   (   get_dict(E, Fs0, X0) -> succ(X0, X)
              ;   X = 1
              ),
              put_dict(E, Fs0, X, Fs)
          ),
          Es,
          freqs{},
          Freqs).

sort_list_mode(M) -->
    frequencies,
    [D, Ps]>>dict_pairs(D, _, Ps),
    sort(2, M),
    [[C-_|_], [C]]>>true.

p1(S) :-
    phrase_from_file(sequence(string, "\n", Rows), 'input/d6.txt'),
    transpose(Rows, Cols),
    maplist([Col, C]>>phrase(sort_list_mode(@>), Col, [C]), Cols, Modes),
    S = Modes.

p2(S) :-
    phrase_from_file(sequence(string, "\n", Rows), 'input/d6.txt'),
    transpose(Rows, Cols),
    maplist([Col, C]>>phrase(sort_list_mode(@<), Col, [C]), Cols, Modes),
    S = Modes.