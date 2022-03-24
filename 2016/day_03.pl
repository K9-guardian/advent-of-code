:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

line([X, Y, Z]) --> blanks, integer(X), blanks, integer(Y), blanks, integer(Z).

triangle_t([X, Y, Z], T) :-
    Z #< X + Y #/\ Z #> abs(X - Y) #<==> B,
    =(B, 1, T).

p1(S) :-
    phrase_from_file(sequence(line, "\n", Ls), 'input/d3.txt'),
    S = length of tfilter(triangle_t) $ Ls.

p2(S) :-
    phrase_from_file(sequence(line, "\n", Ls), 'input/d3.txt'),
    S = length
        of tfilter(triangle_t)
        of maplist_appended(n_list_partitioned(3))
        of transpose
        $ Ls.