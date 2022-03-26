:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).

% I'd rather not parse this.

sample(
    [   [chip-hydrogen, chip-lithium],
        [gen-hydrogen],
        [gen-lithium],
        []
    ]
).

input(
    [   [gen-promethium, chip-promethium],
        [gen-cobalt, gen-curium, gen-ruthenium, gen-plutonium],
        [chip-cobalt, chip-curium, chip-ruthenium, chip-plutonium],
        []
    ]
).

floor_valid([]). % Empty floor is valid.
floor_valid([K-V|Xs]) :-
    (   maplist(=(K), pairs_keys $ Xs), ! % All chips or gens on floor.
    ;   floor_valid_([K-V|Xs])
    ).

floor_valid_([]).
floor_valid_([I|F0]) :-
    (   maplist(=(gen), pairs_keys $ [I|F0]), ! % All gens on floor.
    ;   foldl(select, [chip-E, gen-E], [I|F0], F), % If chip-E on Floor, then gen-E on Floor.
        floor_valid_(F)
    ).

items_floors0_floors([chip-E, gen-E], FF0-TF0, FF-TF) :-
    foldl(select, [chip-E, gen-E], FF0, FF),
    ord_union(TF0, [chip-E, gen-E], TF),
    floor_valid_(TF). % Skip empty and all equal checks.
items_floors0_floors([chip-E, chip-F], FF0-TF0, FF-TF) :-
    foldl(select, [chip-E, chip-F], FF0, FF),
    ord_union(TF0, [chip-E, chip-F], TF),
    floor_valid(TF).
items_floors0_floors([gen-E, gen-F], FF0-TF0, FF-TF) :-
    foldl(select, [gen-E, gen-F], FF0, FF),
    ord_union(TF0, [gen-E, gen-E], TF),
    floor_valid(FF),
    floor_valid(TF).
items_floors0_floors([chip-E], FF0-TF0, FF-TF) :-
    select(chip-E, FF0, FF),
    ord_add_element(TF0, chip-E, TF),
    floor_valid(TF).
items_floors0_floors([gen-E], FF0-TF0, FF-TF) :-
    select(gen-E, FF0, FF),
    ord_add_element(TF0, gen-E, TF),
    floor_valid(TF).

:- table moves/3.

moves(_-[[], [], [], [_|_]]) --> [], !.
moves(1-[F10, F20, F3, F4]) -->
    { items_floors0_floors(Is, F10-F20, F1-F2) },
    [take_from_to(Is, 1, 2)],
    moves(2-[F1, F2, F3, F4]).
moves(2-[F1, F20, F30, F4]) -->
    { items_floors0_floors(Is, F20-F30, F2-F3) },
    [take_from_to(Is, 2, 3)],
    moves(3-[F1, F2, F3, F4]).
moves(2-[F10, F20, F3, F4]) -->
    { items_floors0_floors(Is, F20-F10, F2-F1) },
    [take_from_to(Is, 2, 1)],
    moves(1-[F1, F2, F3, F4]).
moves(3-[F1, F2, F30, F40]) -->
    { items_floors0_floors(Is, F30-F40, F3-F4) },
    [take_from_to(Is, 3, 4)],
    moves(4-[F1, F2, F3, F4]).
moves(3-[F1, F20, F30, F4]) -->
    { items_floors0_floors(Is, F30-F20, F3-F2) },
    [take_from_to(Is, 3, 2)],
    moves(2-[F1, F2, F3, F4]).
moves(4-[F1, F2, F30, F40]) -->
    { items_floors0_floors(Is, F40-F30, F4-F3) },
    [take_from_to(Is, 4, 3)],
    moves(3-[F1, F2, F3, F4]).

p1(S) :-
    length(Ms, _),
    phrase(moves(1-sample(~)), Ms),
    S = Ms.