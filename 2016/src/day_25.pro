:- use_module(lib/assembunny).
:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

p1(S) :-
    phrase_from_file(sequence(instr, "\n", Instrs), 'input/d25.txt'),
    phrase(instrs_optimized_(Instrs), Optd),
    between(1, inf, S),
    instrs_state_(0, _, 1, _, Instrs-Optd, _, [a-S, b-0, c-0, d-0], _, 1, _).
