:- use_module(lib/util).
:- use_module(library(pio)).
:- use_module(library(portray_text)).

:- set_prolog_flag(double_quotes, codes).

instr(swap_pos(X, Y)) --> "swap position ", integer(X), " with position ", integer(Y).
instr(swap_letter(X, Y)) --> "swap letter ", [X], " with letter ", [Y].
instr(rotate_left(X)) --> "rotate left ", integer(X), " step", ("" | "s").
instr(rotate_right(X)) --> "rotate right ", integer(X), " step", ("" | "s").
instr(rotate_based_on(X)) --> "rotate based on position of letter ", [X].
instr(reverse(X, Y)) --> "reverse positions ", integer(X), " through ", integer(Y).
instr(move(X, Y)) --> "move position ", integer(X), " to position ", integer(Y).

instr_state0_state(swap_pos(X, Y), S0, S) :-
    nth0(Y, S0, V),
    nth0(X, S0, T, S1, V),
    nth0(Y, S1, _, S, T).
instr_state0_state(swap_letter(X, Y), S0, S) :-
    selectd(X, S0, 0, S1),
    selectd(Y, S1, X, S2),
    selectd(0, S2, Y, S).
instr_state0_state(rotate_left(X), S0, S) :- n_list_rotated_left(X, S0, S).
instr_state0_state(rotate_right(X), S0, S) :- n_list_rotated_right(X, S0, S).
instr_state0_state(rotate_based_on(X), S0, S) :-
    nth0(I, S0, X),
    if_(I #>= 4, N #= I + 2, N #= I + 1),
    n_list_rotated_right(N, S0, S).
instr_state0_state(reverse(X, Y), S0, S) :-
    length(H, X),
    N #= Y - X + 1, length(M0, N),
    append([H, M0, T], S0),
    reverse(M0, M),
    append([H, M, T], S).
instr_state0_state(move(X, Y), S0, S) :-
    nth0(X, S0, E, S1),
    nth0(Y, S, E, S1).

p1(S) :-
    phrase_from_file(sequence(instr, "\n", Instrs), 'input/d21.txt'),
    foldl(instr_state0_state, Instrs, "abcdefgh", S).

p2(S) :-
    phrase_from_file(sequence(instr, "\n", Instrs), 'input/d21.txt'),
    length(S, 8), S ins 97..104, all_distinct(S),
    foldl(instr_state0_state, Instrs, S, "fbgdceah").
