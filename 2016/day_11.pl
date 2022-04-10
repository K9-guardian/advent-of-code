:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).
:- use_module(library(heaps)).
:- use_module(library(pairs)).
:- use_module(library(ordsets)).

% I'd rather not parse this.

sample([[chip-hydrogen, chip-lithium],
        [gen-hydrogen],
        [gen-lithium],
        []]).

input([[chip-promethium, gen-promethium],
       [gen-cobalt, gen-curium, gen-plutonium, gen-ruthenium],
       [chip-cobalt, chip-curium, chip-plutonium, chip-ruthenium],
       []]).

% Apply the MOST IMPORTANT, ABSOLUTELY ESSENTIAL OPTIMIZATION for pruning states.
floors_normalized(Fs0, Fs) :-
    append(Fs0, Es0),
    maplist([F-E, F-e(E)]>>true, Es0, Es1),
    count_elements_normalized(0, Es1, Es),
    maplist(same_length, Fs0, Fs),
    append(Fs, Es).

count_elements_normalized(X, Es0, Ns) :-
    (   memberchk(gen-e(E), Es0)
    ->  selectchk(gen-e(E), Es0, gen-X, Es1),
        selectchk(chip-e(E), Es1, chip-X, Es),
        count_elements_normalized(succ $ X, Es, Ns)
    ;   Ns = Es0
    ).

% Heuristic for A* search. This is the sum of distances of items from floor 4.
floor_heuristic(F, H) :-
    maplist(length, F, [A, B, C, D]),
    H #= 3 * A + 2 * B + 1 * C + 0 * D.

floor_valid([]).
floor_valid([F-E|FEs]) :-
    (   maplist(=(F), pairs_keys $ FEs), !
    ;   floor_valid_([F-E|FEs])
    ).

floor_valid_([]).
floor_valid_([I|F0]) :-
    (   maplist(=(gen), pairs_keys $ [I|F0]), !
    ;   foldl(selectchk, [chip-E, gen-E], [I|F0], F),
        floor_valid_(F)
    ).

items_floors0_floors([chip-E, gen-E], FF0-TF0, FF-TF) :-
    foldl(selectd, [chip-E, gen-E], FF0, FF),
    foldl([X, S0, S]>>ord_add_element(S0, X, S), [chip-E, gen-E], TF0, TF),
    floor_valid(TF).
items_floors0_floors([chip-E, chip-F], FF0-TF0, FF-TF) :-
    foldl(selectd, [chip-E, chip-F], FF0, FF),
    foldl([X, S0, S]>>ord_add_element(S0, X, S), [chip-E, chip-F], TF0, TF),
    floor_valid(TF).
items_floors0_floors([gen-E, gen-F], FF0-TF0, FF-TF) :-
    foldl(selectd, [gen-E, gen-F], FF0, FF),
    foldl([X, S0, S]>>ord_add_element(S0, X, S), [gen-E, gen-F], TF0, TF),
    floor_valid(FF),
    floor_valid(TF).
items_floors0_floors([chip-E], FF0-TF0, FF-TF) :-
    selectd(chip-E, FF0, FF),
    ord_add_element(TF0, chip-E, TF),
    floor_valid(TF).
items_floors0_floors([gen-E], FF0-TF0, FF-TF) :-
    selectd(gen-E, FF0, FF),
    ord_add_element(TF0, gen-E, TF),
    floor_valid(FF),
    floor_valid(TF).

state0_items_state(1-[F10, F20, F3, F4], Is, 2-[F1, F2, F3, F4]) :-
    items_floors0_floors(Is, F10-F20, F1-F2).
state0_items_state(2-[F1, F20, F30, F4], Is, 3-[F1, F2, F3, F4]) :-
    items_floors0_floors(Is, F20-F30, F2-F3).
state0_items_state(2-[F10, F20, F3, F4], Is, 1-[F1, F2, F3, F4]) :-
    F10 = [_|_],
    items_floors0_floors(Is, F20-F10, F2-F1).
state0_items_state(3-[F1, F2, F30, F40], Is, 4-[F1, F2, F3, F4]) :-
    items_floors0_floors(Is, F30-F40, F3-F4).
state0_items_state(3-[F1, F20, F30, F4], Is, 2-[F1, F2, F3, F4]) :-
    ( F1 = [_|_] ; F20 = [_|_] ),
    items_floors0_floors(Is, F30-F20, F3-F2).
state0_items_state(4-[F1, F2, F30, F40], Is, 3-[F1, F2, F3, F4]) :-
    items_floors0_floors(Is, F40-F30, F4-F3).

queue_seen_dists_(H, S0, Ds0, Ds) :-
    get_from_heap(H, _, E-F, EFs0),
    (   F = [[], [], [], [_|_]]
    ->  Ds = Ds0
    ;   get_assoc(E-(floors_normalized $ F), S0, _)
    ->  queue_seen_dists_(EFs0, S0, Ds0, Ds)
    ;   succ(E, Above), succ(Below, E),
        findall(Above-L, state0_items_state(E-F, [_, _], Above-L), As0),
        findall(Above-L, state0_items_state(E-F, [_], Above-L), As1),
        findall(Below-L, state0_items_state(E-F, [_], Below-L), Bs0),
        findall(Below-L, state0_items_state(E-F, [_, _], Below-L), Bs1),
        ( As0 = [_|_] -> As = As0 ; As = As1 ),
        ( Bs0 = [_|_] -> Bs = Bs0 ; Bs = Bs1 ),
        append(As, Bs, EFs1),
        foldl({S0, D}/[E-F, A0, A]>>
              (   get_assoc(E-(floors_normalized $ F), S0, _)
              ->  A = A0
              ;   put_assoc(E-F, A0, D, A)
              ),
              EFs1,
              Ds0,
              Ds1),
        foldl([E-F, H0, H]>>(floor_heuristic(F, X), add_to_heap(H0, X, E-F, H)), EFs1, EFs0, EFs),
        put_assoc(E-(floors_normalized $ F), S0, _, S),
        get_assoc(E-F, Ds0, D0), succ(D0, D),
        queue_seen_dists_(EFs, S, Ds1, Ds)
    ).

p1(S) :-
    queue_seen_dists_(singleton_heap(~, floor_heuristic $ input(~), 1-input(~)),
                      empty_assoc(~),
                      list_to_assoc $ [1-input(~)-0],
                      Ds),
    gen_assoc(_-[[], [], [], [_|_]], Ds, S).

p2(S) :-
    input([F10, F2, F3, F4]),
    foldl([X, S0, S]>>ord_add_element(S0, X, S),
          [gen-elerium, chip-elerium, gen-dilithium, chip-dilithium],
          F10,
          F1),
    queue_seen_dists_(singleton_heap(~, floor_heuristic $ [F1, F2, F3, F4], 1-[F1, F2, F3, F4]),
                      empty_assoc(~),
                      list_to_assoc $ [1-[F1, F2, F3, F4]-0],
                      Ds),
    gen_assoc(_-[[], [], [], [_|_]], Ds, S).