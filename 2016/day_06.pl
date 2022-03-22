:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

sort_list_mode(M) -->
    frequencies,
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