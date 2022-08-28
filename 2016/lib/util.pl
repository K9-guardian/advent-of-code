:- module(util,
          [(#<)/3,
           (#=)/3,
           (#=<)/3,
           (#>)/3,
           (#>=)/3,
           frequencies/2,
           get_assoc/4,
           list_clumped/2,
           list_deduped/2,
           list_firstdup/2,
           lists_interleaved/3,
           memberd/2,
           n_list_partitioned/3,
           n_list_repeated/3,
           n_list_rotated_left/3,
           n_list_rotated_right/3,
           n_list_split/4,
           nth0/5,
           nth1/5,
           peano_natural/2,
           selectd/3,
           selectd/4,
           state//1,
           state//2,
           update_assoc/4,
           update_assoc/5]).

#<(X, Y, T) :- X #< Y #<==> B, =(B, 1, T).
#=(X, Y, T) :- X #= Y #<==> B, =(B, 1, T).
#=<(X, Y, T) :- X #=< Y #<==> B, =(B, 1, T).
#>(X, Y, T) :- X #> Y #<==> B, =(B, 1, T).
#>=(X, Y, T) :- X #>= Y #<==> B, =(B, 1, T).

state(S), [S] --> [S].
state(S0, S), [S] --> [S0].

list_clumped([], []).
list_clumped([L|Ls], Ps) :- list_clumped_(Ls, L-1, Ps).

list_clumped_([], E, [E]).
list_clumped_([J|Ps0], K-N, Ps) :-
    if_(J = K,
        (X #= 1 + N, Ps = Ps1),
        (X #= 1, Ps = [K-N|Ps1])),
    list_clumped_(Ps0, J-X, Ps1).

frequencies(Es, Freqs) :- phrase((msort, list_clumped), Es, Freqs).

memberd(X, [E|Es]) :- if_(X = E, true, memberd(X, Es)).

peano_natural(0, 0).
peano_natural(s(N), M) :-
    M #> 0,
    M #= M1 + 1,
    peano_natural(N, M1).

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
    NM #= N * M,
    n_list_split(NM, Ls1, Ls, _).

n_list_rotated_left(N, Ls0, Ls) :-
    length(Ls0, X),
    M #= N mod X,
    n_list_split(M, Ls0, P, S),
    append(S, P, Ls).

n_list_rotated_right(N, Ls0, Ls) :-
    length(Ls0, X),
    M #= (X - N) mod X,
    n_list_split(M, Ls0, P, S),
    append(S, P, Ls).

n_list_partitioned(N, Ls0, Ls) :-
    n_list_partitioned_(Ls0, N, Ls).

n_list_partitioned_([], _,  []).
n_list_partitioned_([L|Ls0], N, [P|R]) :-
    n_list_split(N, [L|Ls0], P, S),
    n_list_partitioned_(S, N, R).

% get_assoc/5 for lists
nth0(N, Es0, E0, Es, E) :-
   nth0(N, Es0, E0, Es1),
   nth0(N, Es, E, Es1).

nth1(N, Es0, E0, Es, E) :-
   nth1(N, Es0, E0, Es1),
   nth1(N, Es, E, Es1).

get_assoc(K, A, D, V) :-
    (   get_assoc(K, A, V), !
    ;   V = D
    ).

update_assoc(K, A0, G_2, A) :-
    get_assoc(K, A0, V0),
    call(G_2, V0, V),
    put_assoc(K, A0, V, A).

update_assoc(K, A0, G_2, D, A) :-
    (   get_assoc(K, A0, V0), !
    ;   V0 = D
    ),
    call(G_2, V0, V),
    put_assoc(K, A0, V, A).
