:- module(
    util,
    [   frequencies/2,
        list_firstdup/2,
        lists_interleaved/3,
        n_list_partitioned/3,
        n_list_split/3
    ]
).

:- use_module(library(assoc)).

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

list_firstdup(Ls, E) :-
    empty_assoc(Set),
    list_firstdup_(Ls, E, Set).

list_firstdup_([L|Ls], E, Set0) :-
    (   get_assoc(L, Set0, L)
    ->  E = L
    ;   put_assoc(L, Set0, L, Set),
        list_firstdup_(Ls, E, Set)
    ).

lists_interleaved(As, Bs, Cs) :- phrase(lists_interleaved_(As, Bs), Cs).

lists_interleaved_([], []) --> [].
lists_interleaved_([A|As], []) --> [A], string(As).
lists_interleaved_([], [B|Bs]) --> [B], string(Bs).
lists_interleaved_([A|As], [B|Bs]) --> [A], [B], lists_interleaved_(As, Bs).

n_list_split(N, Ls, P-S) :-
    length(P, N),
    append(P, S, Ls).

n_list_partitioned(N, Ls0, Ls) :-
    n_list_partitioned_(Ls0, N, Ls).

n_list_partitioned_([], _,  []).
n_list_partitioned_([L|Ls0], N, [P|R]) :-
    n_list_split(N, [L|Ls0], P-S),
    n_list_partitioned_(S, N, R).