:- module(
    util,
    [clumped/2,
     frequencies/2,
     list_deduped/2,
     list_firstdup/2,
     lists_interleaved/3,
     memberd/2,
     n_list_partitioned/3,
     n_list_split/4,
     selectd/3,
     update_assoc/5]
).
:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(clpfd)).
:- use_module(library(func)).
:- use_module(library(reif)).
:- use_module(library(yall)).

% Lagged head for argument indexing. Deterministic if V-0 is instantiated.
% remainder_head_item_pairs([], K-V0, K, Ps) :-
%     if_(V0 = 1, Ps = [], (V0 #= V + 1, Ps = [K-V])).
% remainder_head_item_pairs([J-I|Ps0], K-V0, K, Ps) :-
%     if_(V0 = 1,
%         (dif(J, K), Ps = [J-I|Ps0]),
%         (V0 #= V + 1, Ps = [K-V, J-I|Ps0])
%     ).

% item_pairs0_pairs(K, [P|Ps0], Ps) :- remainder_head_item_pairs(Ps0, P, K, Ps).

% clumped(Items, Pairs) :- foldl(item_pairs0_pairs, Items, Pairs, []).

clumped([], []).
clumped([K|Ks], Ps) :-
    clumped(Ks, Ps1),
    if_(Ps1 = [],
        Ps = [K-1],
        (   Ps1 = [J-V0|Ps2],
            if_(J = K,
                (V #= V0 + 1, Ps = [K-V|Ps2]),
                Ps = [K-1, J-V0|Ps2]
            )
        )
    ).

update_assoc(K, A0, G_2, D, A) :-
    (   get_assoc(K, A0, V0), !
    ;   V0 = D
    ),
    put_assoc(K, A0, G_2 $ V0, A).

frequencies(Es, Freqs) :- phrase((msort, clumped), Es, Freqs).

memberd(X, [E|Es]) :- if_(X = E, true, memberd(X, Es)).

% Like selectchk but general.
selectd(X, [E|Es0], Es) :- if_(X = E, Es = Es0, (Es = [E|Es1], selectd(X, Es0, Es1))).

list_deduped_([], E, [E]).
list_deduped_([L|Ls0], E, Ls) :-
    if_(E = L,
        Ls = Ls1,
        Ls = [E|Ls1]
    ),
    list_deduped_(Ls0, L, Ls1).

list_deduped([], []).
list_deduped([L|Ls0], Ls) :- list_deduped_(Ls0, L, Ls).

list_firstdup([L|Ls], E) :- if_(memberd_t(L, Ls), E = L, list_firstdup(Ls, E)).

lists_interleaved(As, Bs, Cs) :- phrase(lists_interleaved_(As, Bs), Cs).

lists_interleaved_([], []) --> [].
lists_interleaved_([A|As], []) --> [A], string(As).
lists_interleaved_([], [B|Bs]) --> [B], string(Bs).
lists_interleaved_([A|As], [B|Bs]) --> [A], [B], lists_interleaved_(As, Bs).

n_list_split(N, Ls, P, S) :-
    length(P, N),
    append(P, S, Ls).

n_list_partitioned(N, Ls0, Ls) :-
    n_list_partitioned_(Ls0, N, Ls).

n_list_partitioned_([], _,  []).
n_list_partitioned_([L|Ls0], N, [P|R]) :-
    n_list_split(N, [L|Ls0], P, S),
    n_list_partitioned_(S, N, R).