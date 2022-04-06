:- module(util,
          [frequencies/2,
           get_assoc/4,
           list_clumped/2,
           list_deduped/2,
           list_firstdup/2,
           lists_interleaved/3,
           memberd/2,
           n_list_partitioned/3,
           n_list_repeated/3,
           n_list_split/4,
           selectd/3,
           selectd/4,
           update_assoc/4,
           update_assoc/5]).

:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(clpfd)).
:- use_module(library(func)).
:- use_module(library(reif)).
:- use_module(library(yall)).

list_clumped(Items, Pairs) :-
    (   nonvar(Items)
    ->  list_to_clumped(Items, Pairs)
    ;   nonvar(Pairs)
    ->  clumped_to_list(Pairs, Items)
    ;   list_to_clumped(Items, Pairs)
    ).

list_to_clumped([], []).
list_to_clumped([L|Ls], Ps) :- list_to_clumped_(Ls, L-1, Ps).

list_to_clumped_([], E, [E]).
list_to_clumped_([J|Ps0], K-N, Ps) :-
    if_(J = K,
        (X #= 1 + N, Ps = Ps1),
        (X #= 1, Ps = [K-N|Ps1])),
    list_to_clumped_(Ps0, J-X, Ps1).

remainder_prevkey([], _).
remainder_prevkey([J-_|_], K) :- dif(J, K).

clumped_to_list(Ps, Ks) :- phrase(clumped_to_list_(Ps), Ks).

clumped_to_list_([]) --> [].
clumped_to_list_([K-V|Ps0]) -->
    { V in 1..sup,
      remainder_prevkey(Ps0, V),
      n_list_repeated(V, [K], Vs) },
    Vs,
    clumped_to_list_(Ps0).

frequencies(Es, Freqs) :- phrase((msort, list_clumped), Es, Freqs).

memberd(X, [E|Es]) :- if_(X = E, true, memberd(X, Es)).

% Like selectchk but general.
selectd(X, [E|Es0], Es) :- if_(X = E, Es = Es0, (Es = [E|Es1], selectd(X, Es0, Es1))).

selectd(X, [E|Es0], Y, Es) :- if_(X = E, Es = [Y|Es0], (Es = [E|Es1], selectd(X, Es0, Y, Es1))).

list_deduped([], []).
list_deduped([L|Ls0], Ls) :- list_deduped_(Ls0, L, Ls).

list_deduped_([], E, [E]).
list_deduped_([L|Ls0], E, Ls) :-
    if_(E = L, Ls = Ls1, Ls = [E|Ls1]),
    list_deduped_(Ls0, L, Ls1).

list_firstdup([L|Ls], E) :- if_(memberd_t(L, Ls), E = L, list_firstdup(Ls, E)).

lists_interleaved(As, Bs, Cs) :- phrase(lists_interleaved_(As, Bs), Cs).

lists_interleaved_([], []) --> [].
lists_interleaved_([A|As], []) --> [A], string(As).
lists_interleaved_([], [B|Bs]) --> [B], string(Bs).
lists_interleaved_([A|As], [B|Bs]) --> [A], [B], lists_interleaved_(As, Bs).

n_list_split(N, Ls, P, S) :-
    length(P, N),
    append(P, S, Ls).

n_list_repeated(N, Ls0, Ls) :-
    length(Ls0, M),
    append(Ls0, Ls1, Ls1),
    n_list_split(~ #= N * M, Ls1, Ls, _).

n_list_partitioned(N, Ls0, Ls) :-
    n_list_partitioned_(Ls0, N, Ls).

n_list_partitioned_([], _,  []).
n_list_partitioned_([L|Ls0], N, [P|R]) :-
    n_list_split(N, [L|Ls0], P, S),
    n_list_partitioned_(S, N, R).

get_assoc(K, A, D, V) :-
    (   get_assoc(K, A, V), !
    ;   V = D
    ).

update_assoc(K, A0, G_2, A) :-
    get_assoc(K, A0, V0),
    put_assoc(K, A0, G_2 $ V0, A).

update_assoc(K, A0, G_2, D, A) :-
    (   get_assoc(K, A0, V0), !
    ;   V0 = D
    ),
    put_assoc(K, A0, G_2 $ V0, A).