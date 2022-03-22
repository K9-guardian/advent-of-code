:- module(
    util,
    [   clumped/2,
        frequencies/2,
        list_firstdup/2,
        lists_interleaved/3,
        maplist_appended/3,
        n_list_partitioned/3,
        n_list_split/4
    ]
).

:- use_module(library(assoc)).

frequencies(Es, Freqs) :- phrase((msort, clumped), Es, Freqs).

item_pairs0_pairs(K, [K-V|Ps0], Ps) :-
    if_(V = 1,
        (   (   Ps0 = []
            ;   Ps0 = [J-_|_],
                dif(K, J)
            ),
            Ps = Ps0
        ),
        (   V #= V0 + 1,
            Ps = [K-V0|Ps0]
        )
    ).

clumped(Items, Pairs) :- foldl(item_pairs0_pairs, Items, Pairs, []).

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

n_list_split(N, Ls, P, S) :-
    length(P, N),
    append(P, S, Ls).

n_list_partitioned(N, Ls0, Ls) :-
    n_list_partitioned_(Ls0, N, Ls).

n_list_partitioned_([], _,  []).
n_list_partitioned_([L|Ls0], N, [P|R]) :-
    n_list_split(N, [L|Ls0], P, S),
    n_list_partitioned_(S, N, R).