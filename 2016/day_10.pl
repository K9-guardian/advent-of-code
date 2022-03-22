:- use_module(lib/double_quotes).
:- use_module(library(pairs)).
:- use_module(lib/pio).
:- use_module(lib/util).

loc(bot(B)) --> "bot ", integer(B).
loc(output(B)) --> "output ", integer(B).

line(value_bot(V, B)) --> "value ", integer(V), " goes to ", loc(B).
line(bot_low_high(B, L, H)) --> loc(B), " gives low to ", loc(L), " and high to ", loc(H).

% Tfw list_to_fdset/2 and empty_set/1 won't load.
list_to_fdset([X|Xs], Set) :- foldl([N, S0, S]>>(S = S0 \/ N), Xs, X, Set).

move_state(value_bot(V, B), S) :-
    get_assoc(B, S, X-Y),
    V #= X #\ V #= Y.

move_state(bot_low_high(_, output(_), output(_)), _).
move_state(bot_low_high(bot(B), bot(L), output(_)), S) :-
    get_assoc(bot(B), S, X-_),
    get_assoc(bot(L), S, Lx-Ly),
    X #= Lx #\ X #= Ly.

move_state(bot_low_high(bot(B), output(_), bot(H)), S) :-
    get_assoc(bot(B), S, _-Y),
    get_assoc(bot(H), S, Hx-Hy),
    Y #= Hx #\ Y #= Hy.

move_state(bot_low_high(bot(B), bot(L), bot(H)), S) :-
    get_assoc(bot(B), S, X-Y),
    get_assoc(bot(L), S, Lx-Ly),
    get_assoc(bot(H), S, Hx-Hy),
    X #= Lx #\ X #= Ly,
    Y #= Hx #\ Y #= Hy.

p1(S) :-
    % phrase_from_file(sequence(line, "\n", Moves), 'input/d10.txt'),
    phrase(sequence(line, "\n", Moves),
"value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2"
),
    call_dcg((convlist([value_bot(V, _), V]>>true), list_to_fdset), Moves, ValueSet),
    convlist([bot_low_high(B, _, _), B]>>true, Moves, Bots),
    same_length(Bots, Values), maplist([X-Y]>>(X #< Y), Values),
    maplist_appended([X-Y, [X, Y]]>>true, Values, ValuesList),
    ValuesList ins ValueSet,
    pairs_keys_values(BotsChips0, Bots, Values), list_to_assoc(BotsChips0, BotsChips),
    maplist({BotsChips}/[M]>>move_state(M, BotsChips), Moves),
    % assoc_to_list(BotsChips, Pairs),
    % include([_-(17-61)]>>true, Pairs, S).
    % member(bot(S)-(17-61), Pairs).
    assoc_to_list(BotsChips, S).
    % assoc_to_list(BotsChips, [_,_-(X-Y)|_]),
    % S = [X, Y],
    % label([X, Y]).