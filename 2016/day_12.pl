:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

% Tfw integer//1 doesn't parse negatives unless you use codes.
int(i(Z)) --> "-", integer(N), { Z #= -N }.
int(i(N)) --> integer(N).

reg(r(a)) --> "a".
reg(r(b)) --> "b".
reg(r(c)) --> "c".
reg(r(d)) --> "d".

instr(cpy(X, Y)) --> "cpy ", (reg(X) | int(X)), " ", reg(r(Y)).
instr(inc(X)) --> "inc ", reg(r(X)).
instr(dec(X)) --> "dec ", reg(r(X)).
instr(jnz(X, Y)) --> "jnz ", (reg(X) | int(X)), " ", int(i(Y)).

% Our input has the pattern to add 2 numbers.
% inc a
% dec b
% jnz b -2
% To optimize this pattern, we replace it with an add instruction.
% To make sure that other jumps are the same, we pad the instructions.
instrs_optimized_([]) --> [].
instrs_optimized_([inc(A), dec(B), jnz(r(B), -2)|Is]) -->
    !,
    [jnz(i(0), 0), jnz(i(0), 0), add(A, B)],
    instrs_optimized_(Is).
instrs_optimized_([dec(B), inc(A), jnz(r(B), -2)|Is]) -->
    !,
    [jnz(i(0), 0), jnz(i(0), 0), add(A, B)],
    instrs_optimized_(Is).
instrs_optimized_([I|Is]) --> [I], instrs_optimized_(Is).

atom_regs_val(i(X), _, X).
atom_regs_val(r(R), Regs, V) :- memberd(R-V, Regs).

mov_state0_state(cpy(X, Y), N0-Regs0, N-Regs) :-
    N #= N0 + 1, atom_regs_val(X, Regs0, V), selectd(Y-_, Regs0, Y-V, Regs).
mov_state0_state(inc(X), N0-Regs0, N-Regs) :-
    N #= N0 + 1, V #= V0 + 1, selectd(X-V0, Regs0, X-V, Regs).
mov_state0_state(dec(X), N0-Regs0, N-Regs) :-
    N #= N0 + 1, V #= V0 - 1, selectd(X-V0, Regs0, X-V, Regs).
mov_state0_state(jnz(X, Y), N0-Regs, N-Regs) :-
    atom_regs_val(X, Regs, V), if_(V = 0, N #= N0 + 1, N #= N0 + Y).
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