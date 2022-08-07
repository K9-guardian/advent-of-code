:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

tile_boolean('.', 0). % Safe
tile_boolean('^', 1). % Trap

% clpfd is faster than clpb lmao
left_center_right_trap(L, C, R, T) :-
    [L, C, R, T] ins 0..1,
    (L * C * (R xor 1))
  + ((L xor 1) * C * R)
  + (L * (C xor 1) * (R xor 1))
  + ((L xor 1) * (C xor 1) * R) #= T.

top_bottom(Ts, Bs) :-
    same_length(Ts, Bs),
    append([[0], Ts, [0]], TsE),
    bottom_top_(Bs, TsE).

bottom_top_([], _).
bottom_top_([B|Bs], [T0, T1, T2|Ts]) :-
    left_center_right_trap(T0, T1, T2, B),
    bottom_top_(Bs, [T1, T2|Ts]).

n_row_safe0_safe(1, Rs, S0, S) :-
    call_dcg((tfilter(=(0)), length), Rs, L),
    S #= S0 + L,
    !.
n_row_safe0_safe(N0, Rs, S0, S) :-
    top_bottom(Rs, Bs),
    call_dcg((tfilter(=(0)), length), Rs, L),
    S1 #= S0 + L,
    N #= N0 - 1,
    n_row_safe0_safe(N, Bs, S1, S).

p1(S) :-
    phrase_from_file(string(Ls0), 'input/d18.txt'),
    maplist(tile_boolean, Ls0, Ls),
    n_row_safe0_safe(40, Ls, 0, S).

p2(S) :-
    phrase_from_file(string(Ls0), 'input/d18.txt'),
    maplist(tile_boolean, Ls0, Ls),
    n_row_safe0_safe(400000, Ls, 0, S).