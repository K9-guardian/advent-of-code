:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).
:- use_module(library(pairs)).
:- use_module(library(ugraphs)).

update_assoc(K, A0, G_2, D, A) :-
    (   get_assoc(K, A0, V0), !
    ;   V0 = D
    ),
    call(G_2, V0, V),
    put_assoc(K, A0, V, A).

loc(bot(B)) --> "bot ", integer(B).
loc(output(O)) --> "output ", integer(O).

move(value_bot(value(V), B)) --> "value ", integer(V), " goes to ", loc(B).
move(bot_low_high(B, L, H)) --> loc(B), " gives low to ", loc(L), " and high to ", loc(H).

move_edges0_edges(value_bot(V, B), Es, [V-B|Es]).
move_edges0_edges(bot_low_high(B, L, H), Es, [B-L, B-H|Es]).

move_instrs0_instrs(value_bot(V, B), Is, [V-B|Is]).
move_instrs0_instrs(bot_low_high(B, L, H), Is, [B-(L-H)|Is]).

assoc_vertex_botcomps0_botcomps(A, value(V), BCs0, BCs) :-
    get_assoc(value(V), A, B),
    update_assoc(B, BCs0, {V}/[Vs, [V|Vs]]>>true, [], BCs).
assoc_vertex_botcomps0_botcomps(A, bot(B), BCs0, BCs) :-
    get_assoc(bot(B), A, L-H),
    get_assoc(bot(B), BCs0, Chips),
    sort(Chips, [Min, Max]),
    update_assoc(L, BCs0, {Min}/[Vs, [Min|Vs]]>>true, [], BCs1),
    update_assoc(H, BCs1, {Max}/[Vs, [Max|Vs]]>>true, [], BCs).
assoc_vertex_botcomps0_botcomps(_, output(_), BCs, BCs).

p1(S) :-
    phrase_from_file(sequence(move, "\n", Moves), 'input/d10.txt'),
    foldl(move_edges0_edges, Moves, [], Edges),
    vertices_edges_to_ugraph([], Edges, Graph),
    call_dcg((foldl(move_instrs0_instrs, Moves), list_to_assoc), [], Assoc),
    top_sort(Graph, TopSorted),
    empty_assoc(BotComps0),
    foldl(assoc_vertex_botcomps0_botcomps(Assoc), TopSorted, BotComps0, BotComps),
    ( Target = [17, 61] ; Target = [61, 17] ),
    gen_assoc(bot(S), BotComps, Target).

p2(S) :-
    phrase_from_file(sequence(move, "\n", Moves), 'input/d10.txt'),
    foldl(move_edges0_edges, Moves, [], Edges),
    vertices_edges_to_ugraph([], Edges, Graph),
    call_dcg((foldl(move_instrs0_instrs, Moves), list_to_assoc), [], Assoc),
    top_sort(Graph, TopSorted),
    empty_assoc(BotComps0),
    foldl(assoc_vertex_botcomps0_botcomps(Assoc), TopSorted, BotComps0, BotComps),
    get_assoc(output(0), BotComps, [X]),
    get_assoc(output(1), BotComps, [Y]),
    get_assoc(output(2), BotComps, [Z]),
    S #= X * Y * Z.