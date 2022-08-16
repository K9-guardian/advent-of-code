:- module(assembunny, [instr//1, instrs_optimized_//1, instrs_state_/10]).

:- use_module(util).
:- use_module(library(edcg)).

:- set_prolog_flag(double_quotes, chars). % Tfw flag is per module.

% Tfw integer//1 doesn't parse negatives unless you use codes.
int(i(Z)) --> "-", integer(N), { Z #= -N }.
int(i(N)) --> integer(N).

reg(r(a)) --> "a".
reg(r(b)) --> "b".
reg(r(c)) --> "c".
reg(r(d)) --> "d".

instr(cpy(X, Y)) --> "cpy ", (reg(X) | int(X)), " ", (reg(Y) | int(Y)).
instr(inc(X)) --> "inc ", reg(r(X)).
instr(dec(X)) --> "dec ", reg(r(X)).
instr(jnz(X, Y)) --> "jnz ", (reg(X) | int(X)), " ", (reg(Y) | int(Y)).
instr(tgl(X)) --> "tgl ", (reg(X) | int(X)).
instr(out(X)) --> "out ", reg(r(X)).

instrs_optimized_([]) --> [].

% Our input has the patten to to multiply 2 numbers.
% cpy b c
% inc a
% dec c
% jnz c -2
% dec d
% jnz d -5
% To optimize this patten, we replace it with an mac (multiply and clear) instruction.
instrs_optimized_([cpy(B, r(C)),
                   inc(A),
                   dec(C),
                   jnz(r(C), i(-2)),
                   dec(D),
                   jnz(r(D), i(-5))|Is]) -->
    !,
    [jnz(i(0), i(0)),
     jnz(i(0), i(0)),
     jnz(i(0), i(0)),
     jnz(i(0), i(0)),
     jnz(i(0), i(0)),
     mac(A, B, C, D)],
    instrs_optimized_(Is).

% Our input has the pattern to add 2 numbers.
% inc a
% dec b
% jnz b -2
% To optimize this pattern, we replace it with an aac (add and clear) instruction.
% aac a b will add the value from register b to register a and clear register b.
% To make sure that other jumps are the same, we pad the instructions with jnz 0 0.
instrs_optimized_([inc(A), dec(B), jnz(r(B), i(-2))|Is]) -->
    !,
    [jnz(i(0), i(0)), jnz(i(0), i(0)), aac(A, B)],
    instrs_optimized_(Is).
instrs_optimized_([dec(B), inc(A), jnz(r(B), i(-2))|Is]) -->
    !,
    [jnz(i(0), i(0)), jnz(i(0), i(0)), aac(A, B)],
    instrs_optimized_(Is).
instrs_optimized_([I|Is]) --> [I], instrs_optimized_(Is).

atom_regs_val(i(X), _, X).
atom_regs_val(r(R), Regs, V) :- memberd(R-V, Regs).

% State is getting to be too much to handle D:
edcg:acc_info(iteration, X, N0, N, N #= N0 + X, 0, _). % Stop program after 100_000 iterations.
edcg:acc_info(line, X, N0, N, N #= N0 + X, 0, _).
edcg:acc_info(instructions,
              K-(V0-V),
              Instrs0-_,
              Instrs-Optd,
              (nth1(K, Instrs0, V0, Instrs, V),
               phrase(instrs_optimized_(Instrs), Optd))).
edcg:acc_info(registers, K-(V0-V), Regs0, Regs, selectd(K-V0, Regs0, K-V, Regs)).
edcg:acc_info(output, X, _, X, true, 1, _).

edcg:pred_info(lookup, 2, [registers]).
edcg:pred_info(mov_state_, 1, [line, instructions, registers, output]).
edcg:pred_info(instrs_state_, 0, [iteration, line, instructions, registers, output]).

mov_tgld(inc(X), dec(X)).
mov_tgld(dec(X), inc(X)).
mov_tgld(tgl(X), inc(X)).
mov_tgld(jnz(X, Y), cpy(X, Y)).
mov_tgld(cpy(X, Y), jnz(X, Y)).

inverse(0, 1).
inverse(1, 0).

lookup(i(X), X) -->> [].
lookup(r(R), V) -->> Regs/registers, memberd(R-V, Regs).

mov_state_(cpy(_, i(_))) -->> [1]:line. % Unexecutable instruction.
mov_state_(cpy(X, r(Y))) -->>
    [1]:line,
    lookup(X, V),
    [Y-(_-V)]:registers.
mov_state_(inc(X)) -->>
    [1]:line,
    { V #= V0 + 1
    },
    [X-(V0-V)]:registers.
mov_state_(dec(X)) -->>
    [1]:line,
    { V #= V0 - 1
    },
    [X-(V0-V)]:registers.
mov_state_(jnz(X, Y)) -->>
    lookup(X, V),
    (   V == 0
    ->  [1]:line
    ;   lookup(Y, N),
        [N]:line
    ).
mov_state_(tgl(X)) -->>
    N/line,
    [1]:line,
    (Instrs-_)/instructions,
    lookup(X, V),
    { M #= N + V,
      length(Instrs, L)
    },
    (   M in 1..L
    ->  { mov_tgld(I0, I)
        },
        [M-(I0-I)]:instructions
    ;   []
    ).
mov_state_(out(X)) -->>
    [1]:line,
    Out/output,
    Regs/registers,
    { memberd(X-V, Regs),
      inverse(Out, V)
    },
    [V]:output.
mov_state_(aac(X, Y)) -->>
    [1]:line,
    Regs/registers,
    { V #= A + B,
      memberd(X-A, Regs),
      memberd(Y-B, Regs)
    },
    [X-(_-V)]:registers,
    [Y-(_-0)]:registers.
mov_state_(mac(X, Y, Z, W)) -->>
    [1]:line,
    Regs/registers,
    lookup(Y, B),
    { V #= A + B * D,
      memberd(X-A, Regs),
      memberd(W-D, Regs)
    },
    [X-(_-V)]:registers,
    [Z-(_-0)]:registers,
    [W-(_-0)]:registers.

instrs_state_ -->>
    N/line,
    I/iteration,
    (_-Optd)/instructions,
    { length(Optd, L)
    },
    (   ( N > L ; I > 100_000 )
    ->  []
    ;   [1]:iteration,
        mov_state_(nth1(N, Optd, ~)),
        instrs_state_
    ).