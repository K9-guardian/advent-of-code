:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

move(rect(M, N)) --> "rect ", integer(M), "x", integer(N).
move(rotate(row, Y, D)) --> "rotate row y=", integer(Y), " by ", integer(D).
move(rotate(col, X, D)) --> "rotate column x=", integer(X), " by ", integer(D).

width(50).
height(6).

grid(G) :-
    findall(X-Y-0, (between(1, height(~), Y), between(1, width(~), X)), Ls),
    list_to_assoc(Ls, G).

grid_list(G, Ls) :-
    call_dcg((assoc_to_list,
              maplist([_-_-V, V]>>true),
              n_list_partitioned(height(~)),
              transpose),
             G,
             Ls).

n_list_rotated(N, Ls0, Ls) :-
    length(Ls0, X),
    n_list_split(~ #= X - N, Ls0, P, S),
    append(S, P, Ls).

move_grid0_grid(rect(M, N), G0, G) :-
    findall(X-Y, (between(1, M, X), between(1, N, Y)), Cs),
    foldl([C, A0, A]>>put_assoc(C, A0, 1, A), Cs, G0, G).

move_grid0_grid(rotate(row, Y0, D), G0, G) :-
    Y #= Y0 + 1, numlist(1, width(~), Xs),
    phrase((maplist({G0}/[X, V]>>get_assoc(X-Y, G0, V)),
            n_list_rotated(D),
            maplist([X, V, X-Y-V]>>true, Xs)),
           Xs,
           Vs),
    foldl([K-V, A0, A]>>put_assoc(K, A0, V, A), Vs, G0, G).

move_grid0_grid(rotate(col, X0, D), G0, G) :-
    X #= X0 + 1, numlist(1, height(~), Ys),
    phrase((maplist({G0}/[Y, V]>>get_assoc(X-Y, G0, V)),
            n_list_rotated(D),
            maplist([Y, V, X-Y-V]>>true, Ys)),
           Ys,
           Vs),
    foldl([K-V, A0, A]>>put_assoc(K, A0, V, A), Vs, G0, G).

char_value(' ', 0).
char_value('#', 1).

p1(S) :-
    phrase_from_file(sequence(move, "\n", Moves), 'input/d8.txt'),
    foldl(move_grid0_grid, Moves, grid(~), G),
    call_dcg((assoc_to_values, sum_list), G, S).

p2(S) :-
    phrase_from_file(sequence(move, "\n", Moves), 'input/d8.txt'),
    foldl(move_grid0_grid, Moves, grid(~), G),
    grid_list(G, Ls),
    maplist([L, Cs]>>
            (maplist(char_value, Cs0, L),
             n_list_partitioned(5, Cs0, Cs1),
             phrase(sequence(string, " ", Cs1), Cs)),
            Ls,
            S),
    maplist([L]>>format('~s~n', [L]), S).