:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).

% I'd rather not parse this.

sample(
    [[chip-hydrogen, chip-lithium],
     [gen-hydrogen],
     [gen-lithium],
     []
    ]
).

input(
    [[gen-promethium, chip-promethium],
     [gen-cobalt, gen-curium, gen-ruthenium, gen-plutonium],
     [chip-cobalt, chip-curium, chip-ruthenium, chip-plutonium],
     []
    ]
).

% Change the elements to numbers to faciliate pruning.
% Ordered by order of chips from floor 1 to 4.
element_num(hydrogen, 1).
element_num(lithium, 2).

element_num(promethium, 1).
element_num(cobalt, 2).
element_num(curium, 3).
element_num(ruthenium, 4).
element_num(plutonium, 5).

% Prioritize moving up and taking 2 items, and if going down taking 1 item.
floor_move_items(1, 2, [_, _]).
floor_move_items(1, 2, [_]).
floor_move_items(2, 3, [_, _]).
floor_move_items(2, 3, [_]).
floor_move_items(2, 1, [_]).
floor_move_items(2, 1, [_, _]).
floor_move_items(3, 4, [_, _]).
floor_move_items(3, 4, [_]).
floor_move_items(3, 2, [_]).
floor_move_items(3, 2, [_, _]).
floor_move_items(4, 3, [_]).
floor_move_items(4, 3, [_, _]).

moves(E0-[F10, F20, F30, F40]) -->
    { floor_move_items(E0, E, Is) },
    [takes_from_to(Is, E0, E)],
    moves(E-[F1, F2, F3, F4]).

p1(S) :-
    maplist(maplist([F-E, F-N]>>element_num(E, N)), input(~), Init),
    S = Init.