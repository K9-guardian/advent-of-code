:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/queue).
:- use_module(lib/util).

:- table input/1.

input(N) :- phrase_from_file(integer(N), 'input/d13.txt').

coord_type(X-Y, T) :-
    S0 #= X * X + 3 * X + 2 * X* Y + Y + Y * Y,
    S #= S0 + input(~),
    if_(popcount(S) mod 2 #= 0, T = open, T = wall).

surrounding(X0-Y, X-Y) :- X #= X0 + 1, X #>= 0, coord_type(X-Y, open).
surrounding(X0-Y, X-Y) :- X #= X0 - 1, X #>= 0, coord_type(X-Y, open).
surrounding(X-Y0, X-Y) :- Y #= Y0 + 1, Y #>= 0, coord_type(X-Y, open).
surrounding(X-Y0, X-Y) :- Y #= Y0 - 1, Y #>= 0, coord_type(X-Y, open).

bfs_ -->
    state(s(Q, _)),
    { head_queue_(31-39, _, Q) },
    !,
    [].
bfs_ -->
    state(s(Q0, Ds0), s(Q, Ds)),
    { head_queue_(X-Y, Q1, Q0),
      findall(I-J, (surrounding(X-Y, I-J), \+ get_assoc(I-J, Ds0, _)), Ns),
      foldl(tail_queue_, Ns, Q1, Q),
      get_assoc(X-Y, Ds0, D0), D #= D0 + 1,
      foldl({D}/[X-Y, A0, A]>>put_assoc(X-Y, A0, D, A), Ns, Ds0, Ds)
    },
    bfs_.

p2_bfs_ -->
    state(s(_, Ds)),
    { max_list $ assoc_to_values $ Ds = 51 },
    !.
p2_bfs_ -->
    state(s(Q0, Ds0), s(Q, Ds)),
    { head_queue_(X-Y, Q1, Q0),
      findall(I-J, (surrounding(X-Y, I-J), \+ get_assoc(I-J, Ds0, _)), Ns),
      foldl(tail_queue_, Ns, Q1, Q),
      get_assoc(X-Y, Ds0, D0), D #= D0 + 1,
      foldl({D}/[X-Y, A0, A]>>put_assoc(X-Y, A0, D, A), Ns, Ds0, Ds)
    },
    p2_bfs_.

p1(S) :-
    phrase(bfs_, [s(singleton_queue $ 1-1, list_to_assoc $ [1-1-0])], [s(_, Ds)]),
    get_assoc(31-39, Ds, S).

p2(S) :-
    phrase(p2_bfs_, [s(singleton_queue $ 1-1, list_to_assoc $ [1-1-0])], [s(_, Ds)]),
    S = length $ tfilter(#>=(50)) $ assoc_to_values $ Ds.
