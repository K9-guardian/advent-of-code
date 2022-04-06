:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).
:- use_module(library(ordsets)).
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

% TODO: A pure version of memberchk. Probably named first_memberd_t.
count_elements_normalized(X, Es0, Ns) :-
    (   memberchk(gen-e(E), Es0)
    ->  selectd(gen-e(E), Es0, gen-X, Es1),
        selectd(chip-e(E), Es1, chip-X, Es),
        count_elements_normalized(succ $ X, Es, Ns)
    ;   Ns = Es0
    ).

% Apply the MOST IMPORTANT, ABSOLUTELY ESSENTIAL OPTIMIZATION for pruning states.
% Replaces the elements with integers in ascending order from bottom to top floor.
floors_normalized(Fs0, Fs) :-
    append(Fs0, Es0),
    maplist([F-E, F-e(E)]>>true, Es0, Es1),
    count_elements_normalized(1, Es1, Es),
    maplist(same_length, Fs0, Fs),
    append(Fs, Es).

fd_int_t(FD, X, T) :- X in FD #<==> B, =(B, 1, T).
zsucc(X0, X) :- X #= X0 + 1.
zpred(X0, X) :- X #= X0 - 1.

state0_state(E0-F0, E-F) :-
    member(E, tfilter(fd_int_t(1..4)) $ [zsucc $ E0, zpred $ E0]),
    % TODO: Add state transitions.

queue_seen_dist([], _, _).
queue_seen_dist([E-F|EFs0], S0, D) :-
    (   F = [[], [], [], [_|_]]
    ->  true
    ;   get_assoc(E-(floors_normalized $ F), S0, _)
    ->  queue_seen_dist(EFs0, S0, D)
    ;   findall(L, state0_state(E-F, L), EFs1),
        append(EFs0, EFs1, EFs),
        foldl([X, A0, A]>>put_assoc(X, A0, X, A),
              maplist([E-F0, E-F]>>floors_normalized(F0, F)) $ EFs1,
              S0,
              S),
        queue_seen_dist(EFs, S, succ $ D)
    ).

p1(S) :-
    floors_normalized(sample(~), Norm),
    S = Norm.