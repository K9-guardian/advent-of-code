:- use_module(library(assoc)).
:- use_module(lib/double_quotes).
:- use_module(lib/pio).

n_list_partitioned(N, Ls0, Ls) :-
    n_list_partitioned_(Ls0, N, Ls).

n_list_partitioned_([], _,  []).
n_list_partitioned_([L|Ls0], N, [P|R]) :-
    length(P, N),
    append(P, S, [L|Ls0]),
    n_list_partitioned_(S, N, R).

move(rect(M, N)) --> "rect ", integer(M), "x", integer(N).
move(rotate(row, Y, D)) --> "rotate row y=", integer(Y), " by ", integer(D).
move(rotate(col, X, D)) --> "rotate column x=", integer(X), " by ", integer(D).

width(50).
height(6).

grid(G) :-
    width(W), height(H),
    findall((X-Y)-0, (between(1, H, Y), between(1, W, X)), Ls),
    list_to_assoc(Ls, G).

grid_list(G, Ls) :-
    height(H),
    call_dcg((assoc_to_list, maplist([(_-_)-V, V]>>true), n_list_partitioned(H), transpose), G, Ls).

n_list_rotated(N, Ls0, Ls) :-
    length(S, N),
    append(P, S, Ls0),
    append(S, P, Ls).

move_grid0_grid(rect(M, N), G0, G) :-
    findall(X-Y, (between(1, M, X), between(1, N, Y)), Cs),
    foldl([C, A0, A]>>put_assoc(C, A0, 1, A), Cs, G0, G).

move_grid0_grid(rotate(row, Y0, D), G0, G) :-
    Y #= Y0 + 1, width(W), numlist(1, W, Xs),
    phrase(
        (   maplist({G0}/[X, V]>>get_assoc(X-Y, G0, V)),
            n_list_rotated(D),
            maplist([X, V, (X-Y)-V]>>true, Xs)
        ),
        Xs,
        Vs
    ),
    foldl([K-V, A0, A]>>put_assoc(K, A0, V, A), Vs, G0, G).

move_grid0_grid(rotate(col, X0, D), G0, G) :-
    X #= X0 + 1, height(H), numlist(1, H, Ys),
    phrase(
        (   maplist({G0}/[Y, V]>>get_assoc(X-Y, G0, V)),
            n_list_rotated(D),
            maplist([Y, V, (X-Y)-V]>>true, Ys)
        ),
        Ys,
        Vs
    ),
    foldl([K-V, A0, A]>>put_assoc(K, A0, V, A), Vs, G0, G).

char_value(' ', 0).
char_value('*', 1).

p1(S) :-
    phrase_from_file(sequence(move, "\n", Moves), 'input/d8.txt'),
    grid(G0),
    foldl(move_grid0_grid, Moves, G0, G),
    call_dcg((assoc_to_values,sum_list), G, S).

p2(S) :-
    phrase_from_file(sequence(move, "\n", Moves), 'input/d8.txt'),
    grid(G0),
    foldl(move_grid0_grid, Moves, G0, G),
    grid_list(G, Ls),
    maplist([L, Cs]>>maplist(char_value, Cs, L), Ls, S),
    maplist([L]>>format('~s~n', [L]), S).