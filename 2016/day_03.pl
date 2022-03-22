:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

line([X, Y, Z]) --> blanks, integer(X), blanks, integer(Y), blanks, integer(Z).

triangle_t([X, Y, Z], T) :-
    Z #< X + Y #/\ Z #> abs(X - Y) #<==> B,
    if_(B = 1, T = true, T = false).

p1(S) :-
    phrase_from_file(sequence(line, "\n", Ls), 'input/d3.txt'),
    tfilter(triangle_t, Ls, Ts),
    length(Ts, S).

p2(S) :-
    phrase_from_file(sequence(line, "\n", Ls0), 'input/d3.txt'),
    transpose(Ls0, Ls1),
    maplist_appended(n_list_partitioned(3), Ls1, Ls),
    tfilter(triangle_t, Ls, Ts),
    length(Ts, S).