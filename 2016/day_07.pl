:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(lib/double_quotes).
:- use_module(library(ordsets)).
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

lists_interleaved([], []) --> [].
lists_interleaved([A|As], []) --> [A], string(As).
lists_interleaved([], [B|Bs]) --> [B], string(Bs).
lists_interleaved([A|As], [B|Bs]) --> [A], [B], lists_interleaved(As, Bs).

abba --> string(_), [A], [B], [B], [A], string(_), { dif(A, B) }.

aba(A, B) --> string(_), [A], [B], [A], string(_), { dif(A, B) }.

tls_supported(S) :-
    phrase(sequence(string, ("[" | "]"), Split), S),
    length(Split, Z),
    Z #= X + Y, X #= (Z + 1) div 2,
    length(As, X), length(Bs, Y),
    phrase(lists_interleaved(As, Bs), Split),
    include([A]>>phrase(abba, A), As, [_|_]),
    include([B]>>phrase(abba, B), Bs, []).

ssl_supported(S) :-
    phrase(sequence(string, ("[" | "]"), Split), S),
    length(Split, Z),
    Z #= X + Y, X #= (Z + 1) div 2,
    length(As, X), length(Bs, Y),
    phrase(lists_interleaved(As, Bs), Split),
    phrase((maplist_appended([A, K]>>findall(C-D, phrase(aba(C, D), A), K)), sort), As, Ks),
    phrase((maplist_appended([B, L]>>findall(D-C, phrase(aba(C, D), B), L)), sort), Bs, Ls),
    ord_intersection(Ks, Ls, [_|_]).

p1(S) :-
    phrase_from_file(sequence(string, "\n", Ls), 'input/d7.txt'),
    include(tls_supported, Ls, Vs),
    length(Vs, S).

p2(S) :-
    phrase_from_file(sequence(string, "\n", Ls), 'input/d7.txt'),
    include(ssl_supported, Ls, Vs),
    length(Vs, S).