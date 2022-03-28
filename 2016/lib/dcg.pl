:- module(
    dcg,
    [blank//0,
     blanks//0,
     digit//1,
     integer//1,
     natural//1,
     sequence//2,
     sequence//3,
     string//1
    ]
).

:- set_prolog_flag(double_quotes, chars).

:- use_module(library(apply)).
:- use_module(double_quotes).
:- use_module(pio).

string([]) --> "".
string([L|Ls]) --> [L], string(Ls).

sequence(G_3, Ls) --> sequence_(Ls, G_3).
sequence(G_3, Sep, Ls) --> sequence_(Ls, G_3, Sep).

sequence_([], _) --> [].
sequence_([L|Ls], G_3) --> call(G_3, L), sequence_(Ls, G_3).

sequence_([], _, _) --> [].
sequence_([L], G_3, _) --> call(G_3, L).
sequence_([L|Ls], G_3, Sep) --> call(G_3, L), Sep, sequence_(Ls, G_3, Sep).

blank --> [V], { char_type(V, space) }.
blanks --> blank | blank, blanks.

digit(0) --> "0".
digit(1) --> "1".
digit(2) --> "2".
digit(3) --> "3".
digit(4) --> "4".
digit(5) --> "5".
digit(6) --> "6".
digit(7) --> "7".
digit(8) --> "8".
digit(9) --> "9".

natural(N) --> sequence(digit, Vs), { horny(Vs, N) }.

horny([L|Ls], N) :- foldl([V, S0, S]>>(S #= S0 * 10 + V), [L|Ls], 0, N).

integer(Z, [L|Ls], Rs) :-
    if_(L = '-',
        (phrase(natural(N), Ls, Rs), Z #= -N),
        phrase(natural(Z), [L|Ls], Rs)
    ).