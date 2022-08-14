:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/queue).
:- use_module(lib/util).
:- use_module(library(assoc)).
:- use_module(library(pairs)).

grid_to_assoc(M, A) :- phrase(grid_to_assoc_(M), [s(0-0, empty_assoc(~))], [s(_-_, A)]).

grid_to_assoc_([]) --> [].
grid_to_assoc_([[]|Rows]) -->
    state(s(X0-_, A), s(X-0, A)),
    { X #= X0 + 1
    },
    grid_to_assoc_(Rows).
grid_to_assoc_([[R|Rs]|Rows]) -->
    state(s(X-Y0, A0), s(X-Y, A)),
    { put_assoc(X-Y0, A0, R, A),
      Y #= Y0 + 1
    },
    grid_to_assoc_([Rs|Rows]).

surrounding(X0-Y, X-Y) :- X #= X0 + 1.
surrounding(X0-Y, X-Y) :- X #= X0 - 1.
surrounding(X-Y0, X-Y) :- Y #= Y0 + 1.
surrounding(X-Y0, X-Y) :- Y #= Y0 - 1.

bfs(Assoc, Xo-Yo, Xd-Yd, D) :-
    once(phrase(bfs_(Assoc, Xd-Yd),
                [s(singleton_queue $ Xo-Yo, list_to_assoc $ [Xo-Yo-0])],
                [s(_, Dists)])),
    get_assoc(Xd-Yd, Dists, D).

bfs_(_, X-Y) -->
    state(s(Q, _)),
    { head_queue_(X-Y, _, Q)
    }.
bfs_(Assoc, Xd-Yd) -->
    state(s(Q0, S0), s(Q, S)),
    { head_queue_(Xo-Yo, Q1, Q0),
      get_assoc(Xo-Yo, S0, D0),
      D #= D0 + 1,
      findall(I-J, surrounding(Xo-Yo, I-J), Ns0),
      exclude({S0}/[N]>>get_assoc(N, S0, _), Ns0, Ns1),
      include({Assoc}/[N]>>(get_assoc(N, Assoc, V), memberd(V, ".01234567")), Ns1, Ns),
      foldl(tail_queue_, Ns, Q1, Q),
      foldl({D}/[N, S0, S]>>put_assoc(N, S0, D, S), Ns, S0, S)
    },
    bfs_(Assoc, Xd-Yd).

list_partitioned([X, Y]) --> [[X, Y]].
list_partitioned([X, Y|Ps]) --> [[X, Y]], list_partitioned([Y|Ps]).

adj_permutation_sum(Adj, P0, S) :-
    phrase(list_partitioned(P0), P),
    foldl({Adj}/[[X, Y], S0, S]>>(get_assoc(X-Y, Adj, N), S #= S0 + N), P, 0, S).

p1(S) :-
    phrase_from_file(sequence(string, "\n", Grid), 'input/d24.txt'),
    grid_to_assoc(Grid, Assoc),
    Marks = "01234567",
    maplist({Assoc}/[M, V]>>gen_assoc(V, Assoc, M), Marks, Vals),
    pairs_keys_values(Pairs0, Marks, Vals),
    maplist([K0-V, K-V]>>atom_number(K0, K), Pairs0, Pairs),
    findall(X-Y, (between(0, 7, X), between(0, 7, Y)), Ps),
    foldl({Assoc, Pairs}/[X-Y, A0, A]>>
          (memberd(X-C0, Pairs),
           memberd(Y-C1, Pairs),
           bfs(Assoc, C0, C1, D),
           put_assoc(X-Y, A0, D, A)),
          Ps,
          empty_assoc(~),
          Adj),
    findall(Sum,
            (permutation([1, 2, 3, 4, 5, 6, 7], Ns),
             adj_permutation_sum(Adj, [0|Ns], Sum)),
            Sums),
    min_list(Sums, S).

p2(S) :-
    phrase_from_file(sequence(string, "\n", Grid), 'input/d24.txt'),
    grid_to_assoc(Grid, Assoc),
    Marks = "01234567",
    maplist({Assoc}/[M, V]>>gen_assoc(V, Assoc, M), Marks, Vals),
    pairs_keys_values(Pairs0, Marks, Vals),
    maplist([K0-V, K-V]>>atom_number(K0, K), Pairs0, Pairs),
    findall(X-Y, (between(0, 7, X), between(0, 7, Y)), Ps),
    foldl({Assoc, Pairs}/[X-Y, A0, A]>>
          (memberd(X-C0, Pairs),
           memberd(Y-C1, Pairs),
           bfs(Assoc, C0, C1, D),
           put_assoc(X-Y, A0, D, A)),
          Ps,
          empty_assoc(~),
          Adj),
    findall(Sum,
            (permutation([1, 2, 3, 4, 5, 6, 7], Ns0),
             append([[0], Ns0, [0]], Ns),
             adj_permutation_sum(Adj, Ns, Sum)),
            Sums),
    min_list(Sums, S).