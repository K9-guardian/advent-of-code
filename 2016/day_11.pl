:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).
:- use_module(library(pairs)).

% I'd rather not parse this.

sample([[chip-hydrogen, chip-lithium],
        [gen-hydrogen],
        [gen-lithium],
        []]).

input([[gen-promethium, chip-promethium],
       [gen-cobalt, gen-curium, gen-ruthenium, gen-plutonium],
       [chip-cobalt, chip-curium, chip-ruthenium, chip-plutonium],
       []]).

% Apply the MOST IMPORTANT, ABSOLUTELY ESSENTIAL OPTIMIZATION for pruning states.
floors_normalized(Fs0, Fs) :-
    append(Fs0, Es0),
    maplist([F-E, F-e(E)]>>true, Es0, Es1),
    count_elements_normalized(1, Es1, Es),
    maplist(same_length, Fs0, Fs),
    append(Fs, Es).

count_elements_normalized(X, Es0, Ns) :-
    (   memberchk(gen-e(E), Es0)
    ->  selectchk(gen-e(E), Es0, gen-X, Es1),
        selectchk(chip-e(E), Es1, chip-X, Es),
        count_elements_normalized(succ $ X, Es, Ns)
    ;   Ns = Es0
    ).

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

items_floors0_floors([chip-E, gen-E], FF0-TF, FF-[chip-E, gen-E|TF]) :-
    foldl(selectd, [chip-E, gen-E], FF0, FF),
    floor_valid(FF),
    floor_valid([chip-E, gen-E|TF]).
items_floors0_floors([chip-E, chip-F], FF0-TF, FF-[chip-E, chip-F|TF]) :-
    foldl(selectd, [chip-E, chip-F], FF0, FF),
    floor_valid(FF),
    floor_valid([chip-E, chip-F|TF]).
items_floors0_floors([gen-E, gen-F], FF0-TF, FF-[gen-E, gen-F|TF]) :-
    foldl(selectd, [gen-E, gen-F], FF0, FF),
    floor_valid(FF),
    floor_valid([gen-E, gen-F|TF]).
items_floors0_floors([chip-E], FF0-TF, FF-[chip-E|TF]) :-
    selectd(chip-E, FF0, FF),
    floor_valid(FF),
    floor_valid([chip-E|TF]).
items_floors0_floors([gen-E], FF0-TF, FF-[gen-E|TF]) :-
    selectd(gen-E, FF0, FF),
    floor_valid(FF),
    floor_valid([gen-E|TF]).

state0_state(1-[F10, F20, F3, F4], 2-[F1, F2, F3, F4]) :-
    items_floors0_floors(_, F10-F20, F1-F2).
state0_state(2-[F1, F20, F30, F4], 3-[F1, F2, F3, F4]) :-
    items_floors0_floors(_, F20-F30, F2-F3).
state0_state(2-[F10, F20, F3, F4], 1-[F1, F2, F3, F4]) :-
    items_floors0_floors(_, F20-F10, F2-F1).
state0_state(3-[F1, F2, F30, F40], 4-[F1, F2, F3, F4]) :-
    items_floors0_floors(_, F30-F40, F3-F4).
state0_state(3-[F1, F20, F30, F4], 2-[F1, F2, F3, F4]) :-
    items_floors0_floors(_, F30-F20, F3-F2).
state0_state(4-[F1, F2, F30, F40], 3-[F1, F2, F3, F4]) :-
    items_floors0_floors(_, F30-F40, F3-F4).

queue_seen_dists_([E-F|EFs0], S0, Ds0, Ds) :-
    format('~k ~k ~k~n', [E, maplist(length) $ F, max_list of assoc_to_values $ Ds0]),
    (   F = [[], [], [], [_|_]]
    ->  Ds = Ds0
    ;   get_assoc(E-(floors_normalized $ F), S0, _)
    ->  queue_seen_dists_(EFs0, S0, Ds0, Ds)
    ;   findall(L, state0_state(E-F, L), EFs1),
        append(EFs0, EFs1, EFs),
        put_assoc(E-(floors_normalized $ F), S0, _, S),
        get_assoc(E-(maplist(sort) $ F), Ds0, D0),
        succ(D0, D),
        foldl({D}/[E-F0, A0, A]>>(maplist(sort, F0, F), put_assoc(E-F, A0, D, A)), EFs1, Ds0, Ds1),
        queue_seen_dists_(EFs, S, Ds1, Ds)
    ).

p1(S) :-
    queue_seen_dists_([1-sample(~)],
                      empty_assoc(~),
                      list_to_assoc $ [1-(maplist(sort) $ sample(~))-0],
                      Ds),
    gen_assoc(_-[[], [], [], [_|_]], Ds, S).
    % S = Ds.