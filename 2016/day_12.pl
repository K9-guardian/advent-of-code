:- use_module(lib/assembunny).
:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

p1(S) :-
    phrase_from_file(sequence(instr, "\n", Instrs), 'input/d12.txt'),
    phrase(instrs_optimized_(Instrs), Optd),
    instrs_state_(0, _, 1, _, Instrs-Optd, _, [a-0, b-0, c-0, d-0], [a-S|_], _, _).

p2(S) :-
    phrase_from_file(sequence(instr, "\n", Instrs), 'input/d12.txt'),
    phrase(instrs_optimized_(Instrs), Optd),
    instrs_state_(0, _, 1, _, Instrs-Optd, _, [a-0, b-0, c-1, d-0], [a-S|_], _, _).
