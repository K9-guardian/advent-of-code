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

moves_edges([]) --> [].
moves_edges([value_bot(V, B)|Ms]) --> [V-B], moves_edges(Ms).
moves_edges([bot_low_high(B, L, H)|Ms]) --> [B-L, B-H], moves_edges(Ms).

moves_instrs([]) --> state(S, S).
moves_instrs([value_bot(V, B)|Ms]) -->
    state(S0, S),
    { put_assoc(V, S0, B, S)
    },
    moves_instrs(Ms).
moves_instrs([bot_low_high(B, L, H)|Ms]) -->
    state(S0, S),
    { put_assoc(B, S0, L-H, S)
    },
    moves_instrs(Ms).

vertices_assoc_botcomps_([], _) --> state(S, S).
vertices_assoc_botcomps_([output(_)|Verts], A) --> vertices_assoc_botcomps_(Verts, A).
vertices_assoc_botcomps_([value(V)|Verts], A) -->
    state(S0, S),
    { get_assoc(value(V), A, B),
      update_assoc(B, S0, {V}/[Vs, [V|Vs]]>>true, [], S)
    },
    vertices_assoc_botcomps_(Verts, A).
vertices_assoc_botcomps_([bot(B)|Verts], A) -->
    state(S0, S),
    { get_assoc(bot(B), A, L-H),
      sort(get_assoc(bot(B), S0, ~), [Min, Max]),
      update_assoc(L, S0, {Min}/[Vs, [Min|Vs]]>>true, [], S1),
      update_assoc(H, S1, {Max}/[Vs, [Max|Vs]]>>true, [], S)
    },
    vertices_assoc_botcomps_(Verts, A).

p1(S) :-
    phrase_from_file(sequence(move, "\n", Moves), 'input/d10.txt'),
    phrase(moves_edges(Moves), Edges),
    phrase(moves_instrs(Moves), [empty_assoc(~)], [Assoc]),
    top_sort(vertices_edges_to_ugraph([], Edges, ~), Ts),
    phrase(vertices_assoc_botcomps_(Ts, Assoc), [empty_assoc(~)], [BotComps]),
    ( Target = [17, 61] ; Target = [61, 17] ),
    gen_assoc(bot(S), BotComps, Target).

p2(S) :-
    phrase_from_file(sequence(move, "\n", Moves), 'input/d10.txt'),
    phrase(moves_edges(Moves), Edges),
    phrase(moves_instrs(Moves), [empty_assoc(~)], [Assoc]),
    top_sort(vertices_edges_to_ugraph([], Edges, ~), Ts),
    phrase(vertices_assoc_botcomps_(Ts, Assoc), [empty_assoc(~)], [BotComps]),
    maplist({BotComps}/[N, O]>>get_assoc(output(N), BotComps, [O]), [0, 1, 2], [X, Y, Z]),
    S #= X * Y * Z.