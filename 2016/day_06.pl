:- use_module(library(assoc)).
:- use_module(lib/double_quotes).
:- use_module(lib/pio).

frequencies(Es, Freqs) :-
    empty_assoc(Freqs0),
    foldl(
        [E, Fs0, Fs]>>
        (   (   get_assoc(E, Fs0, X0)
            ->  succ(X0, X)
            ;   X = 1
            ),
            put_assoc(E, Fs0, X, Fs)
        ),
        Es,
        Freqs0,
        Freqs
    ).

sort_list_mode(M) -->
    frequencies,
    assoc_to_list,
    sort(2, M),
    [[C-_|_], C]>>true.

p1(S) :-
    phrase_from_file(sequence(string, "\n", Rows), 'input/d6.txt'),
    transpose(Rows, Cols),
    maplist([Col, C]>>call_dcg(sort_list_mode(@>), Col, C), Cols, Modes),
    S = Modes.

p2(S) :-
    phrase_from_file(sequence(string, "\n", Rows), 'input/d6.txt'),
    transpose(Rows, Cols),
    maplist([Col, C]>>call_dcg(sort_list_mode(@<), Col, C), Cols, Modes),
    S = Modes.