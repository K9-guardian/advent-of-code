:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

line(rem_mod(R, M)) -->
    "Disc #",
    integer(N),
    " has ",
    integer(M),
    " positions; at time=0, it is at position ",
    integer(S),
    ".",
    { R #= -(N + S) mod M }.

cong_state0_state(rem_mod(R, M), T0-I0, T-I) :-
    N #= T0 mod M,
    if_(N = R,
        (T = T0, I #= I0 * M),
        (T1 #= T0 + I0,
         cong_state0_state(rem_mod(R, M), T1-I0, T-I))).

p1(S) :-
    phrase_from_file(sequence(line, "\n", Ls0), 'input/d15.txt'),
    sort(2, @>=, Ls0, [rem_mod(T0, I)|Ls]),
    foldl(cong_state0_state, Ls, T0-I, T-_),
    S = T.

p2(S) :-
    phrase_from_file(sequence(line, "\n", Ls0), 'input/d15.txt'),
    sort(2, @>=, [rem_mod(~ #= -(succ of length $ Ls0) mod 11, 11)|Ls0], [rem_mod(T0, I)|Ls]),
    foldl(cong_state0_state, Ls, T0-I, T-_),
    S = T.