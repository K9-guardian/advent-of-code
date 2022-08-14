:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/queue).
:- use_module(lib/util).

elves_state0_state(1, Left-Right, Left-Right).
elves_state0_state(N0, Left0-Right0, Left-Right) :-
    head_queue_(_, Right1, Right0), % Steal from middle elf.

    head_queue_(H, Left1, Left0), % Move thief to the end.
    tail_queue_(H, Right1, Right2),

    (   N0 mod 2 #= 1 % Adjust midpoint.
    ->  head_queue_(M, RightE, Right2),
        tail_queue_(M, Left1, LeftE)
    ;   LeftE-RightE = Left1-Right2
    ),

    N #= N0 - 1,
    elves_state0_state(N, LeftE-RightE, Left-Right).

% Josephus problem (https://en.wikipedia.org/wiki/Josephus_problem)
p1(S) :-
    phrase_from_file(integer(N), 'input/d19.txt'),
    2^A + L #= N, [L, A] ins 0..N,
    once(labeling([max(A)], [A, L])),
    S #= 2 * L + 1.

p2(S) :-
    phrase_from_file(integer(N), 'input/d19.txt'),
    numlist(1, N, Elves),
    Mid #= N div 2,
    n_list_split(Mid, Elves, Left0, Right0),
    list_queue(Left0, Left), list_queue(Right0, Right),
    elves_state0_state(N, Left-Right, _-End),
    singleton_queue(S, End).