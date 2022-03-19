:- use_module(lib/double_quotes).
:- use_module(lib/pio).

maplist_appended(G_2, Ls0, Ls) :-
    foldl(
        {G_2}/[E0, Es0, Es]>>
        (   call(G_2, E0, E),
            append(E, Es, Es0)
        ),
        Ls0,
        Ls,
        []
    ).

line([X, Y, Z]) --> blanks, integer(X), blanks, integer(Y), blanks, integer(Z).

triangle_t([X, Y, Z], T) :-
    Z #< X + Y #/\ Z #> abs(X - Y) #<==> B,
    if_(B = 1, T = true, T = false).

n_list_partitioned(N, Ls0, Ls) :-
    n_list_partitioned_(Ls0, N, Ls).

n_list_partitioned_([], _,  []).
n_list_partitioned_([L|Ls0], N, [P|R]) :-
    length(P, N),
    append(P, S, [L|Ls0]),
    n_list_partitioned_(S, N, R).

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