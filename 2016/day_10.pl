:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).
:- use_module(library(pairs)).
:- use_module(library(ugraphs)).

loc(bot(B)) --> "bot ", integer(B).
loc(output(O)) --> "output ", integer(O).
loc(value(V)) --> "value ", integer(V).

move(value_bot(V, B)) --> loc(V), " goes to ", loc(B).
move(bot_low_high(B, L, H)) --> loc(B), " gives low to ", loc(L), " and high to ", loc(H).

move_edges0_edges(value_bot(V, B), Es, [V-B|Es]).
move_edges0_edges(bot_low_high(B, L, H), Es, [B-L, B-H|Es]).

move_instrs0_instrs(value_bot(V, B), Is, [V-B|Is]).
move_instrs0_instrs(bot_low_high(B, L, H), Is, [B-(L-H)|Is]).

vertex_assoc_botcomps0_botcomps(output(_), _, BCs, BCs).
vertex_assoc_botcomps0_botcomps(value(V), A, BCs0, BCs) :-
    get_assoc(value(V), A, B),
    update_assoc(B, BCs0, {V}/[Vs, [V|Vs]]>>true, [], BCs).
vertex_assoc_botcomps0_botcomps(bot(B), A, BCs0, BCs) :-
    get_assoc(bot(B), A, L-H),
    sort(get_assoc(bot(B), BCs0, ~), [Min, Max]),
    update_assoc(L, BCs0, {Min}/[Vs, [Min|Vs]]>>true, [], BCs1),
    update_assoc(H, BCs1, {Max}/[Vs, [Max|Vs]]>>true, [], BCs).

p1(S) :-
    phrase_from_file(sequence(move, "\n", Moves), 'input/d10.txt'),
    foldl(move_edges0_edges, Moves, [], Edges),
    top_sort(vertices_edges_to_ugraph([], Edges, ~), Ts),
    call_dcg((foldl(move_instrs0_instrs, Moves), list_to_assoc), [], Assoc),
    foldl({Assoc}/[V, BCs0, BCs]>>vertex_assoc_botcomps0_botcomps(V, Assoc, BCs0, BCs),
          Ts,
          empty_assoc(~),
          BotComps),
    ( Target = [17, 61] ; Target = [61, 17] ),
    gen_assoc(bot(S), BotComps, Target).

p2(S) :-
    phrase_from_file(sequence(move, "\n", Moves), 'input/d10.txt'),
    foldl(move_edges0_edges, Moves, [], Edges),
    top_sort(vertices_edges_to_ugraph([], Edges, ~), Ts),
    call_dcg((foldl(move_instrs0_instrs, Moves), list_to_assoc), [], Assoc),
    foldl({Assoc}/[V, BCs0, BCs]>>vertex_assoc_botcomps0_botcomps(V, Assoc, BCs0, BCs),
          Ts,
          empty_assoc(~),
          BotComps),
    maplist({BotComps}/[N, O]>>get_assoc(output(N), BotComps, [O]), [0, 1, 2], [X, Y, Z]),
    S #= X * Y * Z.