:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(library(yall)).

maplist_appended(G_2, Ls0, Ls) :-
    foldl({G_2}/[E0, Es0, Es]>>
          (   call(G_2, E0, E),
              append(E, Es, Es0)
          ),
          Ls0,
          Ls,
          []).

line([X, Y, Z]) --> blanks, integer(X), blanks, integer(Y), blanks, integer(Z).

triangle_t([X, Y, Z], T) :-
    Z #< X + Y #/\ Z #> abs(X - Y) #<==> B,
    if_(B = 1, T = true, T = false).

list_partitioned(N, Ls0, Ls) :-
    list_partitioned_(Ls0, N, Ls).

list_partitioned_([], _,  []).
list_partitioned_([L|Ls0], N, [P|R]) :-
    length(P, N),
    append(P, S, [L|Ls0]),
    list_partitioned_(S, N, R).

p1(S) :-
    phrase_from_file(sequence(line, "\n", Ls), 'input/d3.txt'),
    tfilter(triangle_t, Ls, Ts),
    length(Ts, S).

p2(S) :-
    phrase_from_file(sequence(line, "\n", Ls0), 'input/d3.txt'),
    transpose(Ls0, Ls1),
    maplist_appended(list_partitioned(3), Ls1, Ls),
    tfilter(triangle_t, Ls, Ts),
    length(Ts, S).