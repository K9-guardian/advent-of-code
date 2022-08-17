:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

:- table input/1.

input(N) :- phrase_from_file(integer(N), 'input/d13.txt').

coord_type(X-Y, T) :-
    S0 #= X * X + 3 * X + 2 * X* Y + Y + Y * Y,
    S #= S0 + input(~),
    if_(popcount(S) mod 2 #= 0, T = open, T = wall).

coord0_coord(X0-Y, X-Y) :- X #= X0 + 1, X #>= 0, coord_type(X-Y, open).
coord0_coord(X0-Y, X-Y) :- X #= X0 - 1, X #>= 0, coord_type(X-Y, open).
coord0_coord(X-Y0, X-Y) :- Y #= Y0 + 1, Y #>= 0, coord_type(X-Y, open).
coord0_coord(X-Y0, X-Y) :- Y #= Y0 - 1, Y #>= 0, coord_type(X-Y, open).

queue_dists_([31-39|_]) --> !, [].
queue_dists_([X-Y|Ps0]) -->
    state(S0, S),
    { findall(I-J, coord0_coord(X-Y, I-J), Ps1),
      exclude({S0}/[X]>>get_assoc(X, S0, _), Ps1, Ps2),
      append(Ps0, Ps2, Ps),
      get_assoc(X-Y, S0, D0), succ(D0, D),
      foldl({D}/[X-Y, A0, A]>>put_assoc(X-Y, A0, D, A), Ps2, S0, S)
    },
    queue_dists_(Ps).

p2_queue_dists_([_|_]) -->
    state(S),
    { max_list $ assoc_to_values $ S = 51
    },
    !.
p2_queue_dists_([X-Y|Ps0]) -->
    state(S0, S),
    { findall(I-J, coord0_coord(X-Y, I-J), Ps1),
      exclude({S0}/[X]>>get_assoc(X, S0, _), Ps1, Ps2),
      append(Ps0, Ps2, Ps),
      get_assoc(X-Y, S0, D0), succ(D0, D),
      foldl({D}/[X-Y, A0, A]>>put_assoc(X-Y, A0, D, A), Ps2, S0, S)
    },
    p2_queue_dists_(Ps).

p1(S) :-
    phrase(queue_dists_([1-1]), [list_to_assoc $ [1-1-0]], [Ds]),
    get_assoc(31-39, Ds, S).

p2(S) :-
    phrase(p2_queue_dists_([1-1]), [list_to_assoc $ [1-1-0]], [Ds]),
    assoc_to_list(Ds, Ls0),
    tfilter([_-_-D]>>(D #=< 50), Ls0, Ls),
    length(Ls, S).