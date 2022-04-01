:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

marker(D, R) --> "(", integer(D), "x", integer(R), ")".

n_list_repeated(N, Ls0, Ls) :-
    length(Ls0, M),
    append(Ls0, Ls1, Ls1),
    n_list_split(~ #= N * M, Ls1, Ls, _).

string_decompressed("", "").
string_decompressed([C|Cs0], Cs) :-
    if_(C = '(',
        (phrase(marker(D, R), [C|Cs0], More),
         n_list_split(D, More, Run0, Rest0),
         n_list_repeated(R, Run0, Run),
         append(Run, Rest, Cs),
         string_decompressed(Rest0, Rest)),
        (Cs = [C|Cs1], string_decompressed(Cs0, Cs1))).

string_decomplength("", 0).
string_decomplength([C|Cs0], N) :-
    if_(C = '(',
        (phrase(marker(D, R), [C|Cs0], More),
         n_list_split(D, More, Run, Rest),
         N #= N0 * R + N1,
         string_decomplength(Run, N0),
         string_decomplength(Rest, N1)),
        (N #= N0 + 1, string_decomplength(Cs0, N0))).

p1(S) :-
    phrase_from_file(string(Cs), 'input/d9.txt'),
    S = length of string_decompressed $ Cs.

p2(S) :-
    phrase_from_file(string(Cs), 'input/d9.txt'),
    string_decomplength(Cs, S).