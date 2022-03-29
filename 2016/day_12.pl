:- use_module(lib/dcg).
:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

reg(a) --> "a".
reg(b) --> "b".
reg(c) --> "c".
reg(d) --> "d".

instr(cpy(X, Y)) --> "cpy ", (reg(X) | integer(X)), " ", reg(Y).
instr(inc(X)) --> "inc ", reg(X).
instr(dec(X)) --> "dec ", reg(X).
instr(jnz(X, Y)) --> "jnz ", (reg(X) | integer(X)), " ", integer(Y).

get_assoc(K, A, D, V) :-
    (   get_assoc(K, A, V), !
    ;   V = D
    ).

mov_state0_state(cpy(X, Y), N0-Regs0, N-Regs) :-
    N #= N0 + 1, get_assoc(X, Regs0, X, V), put_assoc(Y, Regs0, V, Regs).
mov_state0_state(inc(X), N0-Regs0, N-Regs) :-
    N #= N0 + 1, update_assoc(X, Regs0, [V0, V]>>(V #= V0 + 1), Regs).
mov_state0_state(dec(X), N0-Regs0, N-Regs) :-
    N #= N0 + 1, update_assoc(X, Regs0, [V0, V]>>(V #= V0 - 1), Regs).
mov_state0_state(jnz(X, Y), N0-Regs, N-Regs) :-
    get_assoc(X, Regs, X, V), if_(V = 0, N #= N0 + 1, N #= N0 + Y).

#>(X, Y, T) :- X #> Y #<==> B, =(B, 1, T).

instrs_state0_state(Instrs, N-Regs, S) :-
    if_(N #> length of assoc_to_list $ Instrs,
        S = N-Regs,
        (   mov_state0_state(get_assoc(N, Instrs, ~), N-Regs, S1),
            instrs_state0_state(Instrs, S1, S)
        )
    ).

p1(S) :-
    phrase_from_file(sequence(instr, "\n", Lines), 'input/d12.txt'),
    list_to_assoc(pairs_keys_values(~, numlist(1, length $ Lines, ~), Lines), Instrs),
    instrs_state0_state(Instrs, 1-(list_to_assoc $ [a-0, b-0, c-0, d-0]), _-Regs),
    get_assoc(a, Regs, S).
    % assoc_to_list(Instrs, S).