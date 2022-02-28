file_parsed(File, Parsed) :-
    read_file_to_string(File, Input, []),
    split_string(Input, "\n", "\n", Parsed0),
    maplist(string_chars, Parsed0, Parsed).

coord_button(X-Y, B) :- B #= Y * 3 + X + 1.

move_coord_('U', X-Y0, X-Y) :- Y #= max(Y0 - 1, 0).
move_coord_('R', X0-Y, X-Y) :- X #= min(X0 + 1, 2).
move_coord_('D', X-Y0, X-Y) :- Y #= min(Y0 + 1, 2).
move_coord_('L', X0-Y, X-Y) :- X #= max(X0 - 1, 0).

%     1
%   2 3 4
% 5 6 7 8 9
%   A B C
%     D

p2_coord_button(2-0, 1).
p2_coord_button(1-1, 2).
p2_coord_button(2-1, 3).
p2_coord_button(3-1, 4).
p2_coord_button(0-2, 5).
p2_coord_button(1-2, 6).
p2_coord_button(2-2, 7).
p2_coord_button(3-2, 8).
p2_coord_button(4-2, 9).
p2_coord_button(1-3, 'A').
p2_coord_button(2-3, 'B').
p2_coord_button(3-3, 'C').
p2_coord_button(2-4, 'D').

move_unit('U', 0-(-1)).
move_unit('R', 1-0).
move_unit('D', 0-1).
move_unit('L', (-1)-0).

p2_move_coord_(M, X0-Y0, C) :-
    move_unit(M, Xshift-Yshift),
    X #= X0 + Xshift,
    Y #= Y0 + Yshift,
    Dist = abs(X - 2) + abs(Y - 2),
    Dist #=< 2 #<==> B,
    if_(B = 1, C = X-Y, C = X0-Y0).

moves_coord_buttons([], _, []).
moves_coord_buttons([Move|Moves], X0-Y0, [B|Bs]) :-
    foldl(move_coord_, Move, X0-Y0, X-Y),
    coord_button(X-Y, B),
    moves_coord_buttons(Moves, X-Y, Bs).

p2_moves_coord_buttons([], _, []).
p2_moves_coord_buttons([Move|Moves], X0-Y0, [B|Bs]) :-
    foldl(p2_move_coord_, Move, X0-Y0, X-Y),
    p2_coord_button(X-Y, B),
    p2_moves_coord_buttons(Moves, X-Y, Bs).

p1(S) :-
    file_parsed('input/d2.txt', Moves),
    moves_coord_buttons(Moves, 1-1, Bs),
    atomics_to_string(Bs, S).

p2(S) :-
    file_parsed('input/d2.txt', Moves),
    p2_moves_coord_buttons(Moves, 1-1, Bs),
    atomics_to_string(Bs, S).