:- use_module(lib/double_quotes).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(lib/pio).
:- use_module(lib/util).

loc(bot(B)) --> "bot ", integer(B).
loc(output(B)) --> "output ", integer(B).

line(B-value(V)) --> "value ", integer(V), " goes to ", loc(B).
line(B-low_high(L, H)) --> loc(B), " gives low to ", loc(L), " and high to ", loc(H).

moves_botchips0_botchips(BotMoves, BotChips0, BotChips) :-
    once(gen_assoc(bot(B), BotChips0, [Min, Max])),
    get_assoc(bot(B), BotMoves, low_high(L, H)),
    ( get_assoc(L, BotChips0, Ls0), ! ; Ls0 = [] ),
    ( get_assoc(H, BotChips0, Hs0), ! ; Hs0 = [] ),
    ord_add_element(Ls0, Min, Ls),
    ord_add_element(Hs0, Max, Hs),
    del_assoc(bot(B), BotChips0, _, BotChips1),
    put_assoc(L, BotChips1, Ls, BotChips2),
    put_assoc(H, BotChips2, Hs, BotChips).

p1(S) :-
    phrase_from_file(sequence(line, "\n", Lines), 'input/d10.txt'),
    partition([_-value(_)]>>true, Lines, ValueLines, BotLines),
    list_to_assoc(BotLines, BotMoves),
    call_dcg((msort, group_pairs_by_key, ord_list_to_assoc), ValueLines, BotChips0),
    Target = [value(17), value(61)],
    check_iterate_init_(
        [BCs]>>gen_assoc(_, BCs, Target),
        moves_botchips0_botchips(BotMoves),
        BotChips0,
        BotChips
    ),
    assoc_to_list(BotChips, BCs),
    member(bot(S)-Target, BCs).

p2(S) :-
    phrase_from_file(sequence(line, "\n", Lines), 'input/d10.txt'),
    partition([_-value(_)]>>true, Lines, ValueLines, BotLines),
    list_to_assoc(BotLines, BotMoves),
    call_dcg((msort, group_pairs_by_key, ord_list_to_assoc), ValueLines, BotChips0),
    check_iterate_init_(
        [BCs]>>(\+ gen_assoc(bot(_), BCs, _)),
        moves_botchips0_botchips(BotMoves),
        BotChips0,
        BotChips
    ),
    assoc_to_list(BotChips, Ls), writeln(Ls),
    get_assoc(output(0), BotChips, [value(X)]),
    get_assoc(output(1), BotChips, [value(Y)]),
    get_assoc(output(2), BotChips, [value(Z)]),
    S #= X * Y * Z.