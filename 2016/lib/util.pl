:- module(
    util,
    [   clumped/2,
        frequencies/2,
        list_firstdup/2,
        lists_interleaved/3,
        maplist_appended/3,
        memberd/2,
        n_list_partitioned/3,
        n_list_split/4,
        update_assoc/5
    ]
).
:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(clpfd)).
:- use_module(library(func)).
:- use_module(library(reif)).
:- use_module(library(yall)).

item_pairs0_pairs(K, [K-V0|Ps0], Ps) :-
    zcompare(C, 1, V0),
    encoding_remainder_pairs_(C, K-V0, Ps0, Ps).

remainder_key([], _).
remainder_key([J-_|_], K) :- dif(K, J).

encoding_remainder_pairs_(=, K-_, Ps, Ps) :- remainder_key(Ps, K).
encoding_remainder_pairs_(<, K-V0, Ps, [K-V|Ps]) :- V0 #= V + 1.

% TODO: Make this deterministic.
clumped(Items, Pairs) :- foldl(item_pairs0_pairs, Items, Pairs, []).

update_assoc(K, A0, G_2, D, A) :-
    (   get_assoc(K, A0, V0), !
    ;   V0 = D
    ),
    put_assoc(K, A0, G_2 $ V0, A).

frequencies(Es, Freqs) :-
    foldl([K, Fs0, Fs]>>update_assoc(K, Fs0, succ, 0, Fs), Es, empty_assoc(~), Freqs0),
    assoc_to_list(Freqs0, Freqs).

memberd(X, [E|Es]) :- if_(X = E, true, memberd(X, Es)).

list_firstdup([L|Ls], E) :- if_(memberd_t(L, Ls), E = L, list_firstdup(Ls, E)).

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