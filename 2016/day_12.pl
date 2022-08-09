:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

int(int(Z)) --> "-", integer(N), { Z #= -N }.
int(int(N)) --> integer(N).

reg(reg(a)) --> "a".
reg(reg(b)) --> "b".
reg(reg(c)) --> "c".
reg(reg(d)) --> "d".

instr(cpy(X, Y)) --> "cpy ", (reg(X) | int(X)), " ", reg(reg(Y)).
instr(inc(X)) --> "inc ", reg(reg(X)).
instr(dec(X)) --> "dec ", reg(reg(X)).
instr(jnz(X, Y)) --> "jnz ", (reg(X) | int(X)), " ", int(int(Y)).

% Our input has the pattern to add 2 numbers.
% inc a
% dec b
% jnz b -2
% To optimize this pattern, we replace it with an add instruction.
% To make sure that other jumps are the same, we pad the instructions.
instrs_optimized_([]) --> [].
instrs_optimized_([inc(A), dec(B), jnz(reg(B), -2)|Is]) -->
    !,
    [jnz(int(0), 0), jnz(int(0), 0), add(A, B)],
    instrs_optimized_(Is).
instrs_optimized_([dec(B), inc(A), jnz(reg(B), -2)|Is]) -->
    !,
    [jnz(int(0), 0), jnz(int(0), 0), add(A, B)],
    instrs_optimized_(Is).
instrs_optimized_([I|Is]) --> [I], instrs_optimized_(Is).

in_regs_val(int(X), _, X).
in_regs_val(reg(R), Regs, V) :- memberd(R-V, Regs).

mov_state0_state(cpy(X, Y), N0-Regs0, N-Regs) :-
    N #= N0 + 1, in_regs_val(X, Regs0, V), selectd(Y-_, Regs0, Y-V, Regs).
mov_state0_state(inc(X), N0-Regs0, N-Regs) :-
    N #= N0 + 1, V #= V0 + 1, selectd(X-V0, Regs0, X-V, Regs).
mov_state0_state(dec(X), N0-Regs0, N-Regs) :-
    N #= N0 + 1, V #= V0 - 1, selectd(X-V0, Regs0, X-V, Regs).
mov_state0_state(jnz(X, Y), N0-Regs, N-Regs) :-
    in_regs_val(X, Regs, V),
    if_(V = 0, N #= N0 + 1, N #= N0 + Y).
mov_state0_state(add(X, Y), N0-Regs0, N-Regs) :-
    N #= N0 + 1,
    memberd(X-V0, Regs0),
    memberd(Y-V1, Regs0),
    V #= V0 + V1,
    selectd(X-_, Regs0, X-V, Regs1),
    selectd(Y-_, Regs1, Y-0, Regs).

instrs_state0_state(Instrs, N-Regs, S) :-
    length(Instrs, L),
    (   N > L -> S = N-Regs
    ;   mov_state0_state(nth1(N, Instrs, ~), N-Regs, S1),
        instrs_state0_state(Instrs, S1, S)
    ).

p1(S) :-
    phrase_from_file(sequence(instr, "\n", Instrs0), 'input/d12.txt'),
    phrase(instrs_optimized_(Instrs0), Instrs),
    instrs_state0_state(Instrs, 1-[a-0, b-0, c-0, d-0], _-[a-S|_]).

p2(S) :-
    phrase_from_file(sequence(instr, "\n", Instrs0), 'input/d12.txt'),
    phrase(instrs_optimized_(Instrs0), Instrs),
    instrs_state0_state(Instrs, 1-[a-0, b-0, c-1, d-0], _-[a-S|_]).