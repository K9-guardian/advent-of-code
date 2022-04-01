:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

% Tfw integer//1 only checks sign when you're using codes.
int(Z, [L|Ls], Rs) :-
    if_(L = '-',
        (phrase(integer(N), Ls, Rs), Z #= -N),
        phrase(integer(Z), [L|Ls], Rs)).

reg(a) --> "a".
reg(b) --> "b".
reg(c) --> "c".
reg(d) --> "d".

instr(cpy(X, Y)) --> "cpy ", (reg(X) | int(X)), " ", reg(Y).
instr(inc(X)) --> "inc ", reg(X).
instr(dec(X)) --> "dec ", reg(X).
instr(jnz(X, Y)) --> "jnz ", (reg(X) | int(X)), " ", int(Y).

mov_instrs_state0_state(cpy(X, Y), _, N0-Regs0, N-Regs) :-
    N #= N0 + 1, get_assoc(X, Regs0, X, V), put_assoc(Y, Regs0, V, Regs).
mov_instrs_state0_state(inc(X), _, N0-Regs0, N-Regs) :-
    N #= N0 + 1, update_assoc(X, Regs0, [V0, V]>>(V #= V0 + 1), Regs).
mov_instrs_state0_state(dec(X), _, N0-Regs0, N-Regs) :-
    N #= N0 + 1, update_assoc(X, Regs0, [V0, V]>>(V #= V0 - 1), Regs).
mov_instrs_state0_state(jnz(X, Y), Instrs, N0-Regs0, N-Regs) :-
    get_assoc(X, Regs0, X, V),
    (   V == 0
    ->  N #= N0 + 1, Regs = Regs0
    ;   (   Y == -2
        ->  get_assoc(~ #= N0 - 2, Instrs, inc(A)),
            get_assoc(~ #= N0 - 1, Instrs, dec(B)),
            S #= get_assoc(A, Regs0, ~) + get_assoc(B, Regs0, ~),
            put_assoc(A, Regs0, S, Regs1),
            put_assoc(B, Regs1, 0, Regs),
            N #= N0 + 1
        ;   N #= N0 + Y, Regs = Regs0
        )
    ).

instrs_state0_state(Instrs, N-Regs, S) :-
    (   N #> length of assoc_to_list $ Instrs
    ->  S = N-Regs
    ;   mov_instrs_state0_state(get_assoc(N, Instrs, ~), Instrs, N-Regs, S1),
        instrs_state0_state(Instrs, S1, S)
    ).

p1(S) :-
    phrase_from_file(sequence(instr, "\n", Lines), 'input/d12.txt'),
    list_to_assoc(pairs_keys_values(~, numlist(1, length $ Lines, ~), Lines), Instrs),
    instrs_state0_state(Instrs, 1-(list_to_assoc $ [a-0, b-0, c-0, d-0]), _-Regs),
    get_assoc(a, Regs, S).

p2(S) :-
    phrase_from_file(sequence(instr, "\n", Lines), 'input/d12.txt'),
    list_to_assoc(pairs_keys_values(~, numlist(1, length $ Lines, ~), Lines), Instrs),
    instrs_state0_state(Instrs, 1-(list_to_assoc $ [a-0, b-0, c-1, d-0]), _-Regs),
    get_assoc(a, Regs, S).