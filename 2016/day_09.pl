:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

marker(D, R) --> "(", integer(D), "x", integer(R), ")".

n_list_repeated(N, Ls0, Ls) :-
    length(Ls0, M),
    NM #= N * M,
    length(Ls, NM),
    append(Ls0, Ls1, Ls1),
    append(Ls, _, Ls1).

string_decompressed("", "").
string_decompressed([C|Cs0], Cs) :-
    if_(
        C = '(',
        (   phrase(marker(D, R), [C|Cs0], More),
            length(Run, D), append(Run, Rest0, More),
            n_list_repeated(R, Run, Runs),
            append(Runs, Rest, Cs),
            string_decompressed(Rest0, Rest)
        ),
        (   Cs = [C|Cs1],
            string_decompressed(Cs0, Cs1)
        )
    ).

string_decomplength("", 0).
string_decomplength([C|Cs0], N) :-
    if_(
        C = '(',
        (   phrase(marker(D, R), [C|Cs0], More),
            length(Run, D), append(Run, Rest, More),
            N #= N0 * R + N1,
            string_decomplength(Run, N0),
            string_decomplength(Rest, N1)
        ),
        (   N #= N0 + 1,
            string_decomplength(Cs0, N0)
        )
    ).

p1(S) :-
    phrase_from_file(string(Cs0), 'input/d9.txt'),
    string_decompressed(Cs0, Cs),
    length(Cs, S).

p2(S) :-
    phrase_from_file(string(Cs0), 'input/d9.txt'),
    string_decomplength(Cs0, S).