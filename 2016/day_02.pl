:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

move(Ms) --> sequence(turn, Ms).

turn('U') --> "U".
turn('R') --> "R".
turn('D') --> "D".
turn('L') --> "L".

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

p2_coord_button(2-0, '1').
p2_coord_button(1-1, '2').
p2_coord_button(2-1, '3').
p2_coord_button(3-1, '4').
p2_coord_button(0-2, '5').
p2_coord_button(1-2, '6').
p2_coord_button(2-2, '7').
p2_coord_button(3-2, '8').
p2_coord_button(4-2, '9').
p2_coord_button(1-3, 'A').
p2_coord_button(2-3, 'B').
p2_coord_button(3-3, 'C').
p2_coord_button(2-4, 'D').

move_unit('U', 0-(-1)).
move_unit('R', 1-0).
move_unit('D', 0-1).
move_unit('L', (-1)-0).

p2_move_coord_(M, X0-Y0, C) :-
    move_unit(M, I-J),
    X #= X0 + I,
    Y #= Y0 + J,
    Dist #= abs(X - 2) + abs(Y - 2),
    Dist #=< 2 #<==> B,
    if_(B = 1, C = X-Y, C = X0-Y0).

p1(S) :-
    phrase_from_file(sequence(move, "\n", Moves), 'input/d2.txt'),
    foldl(
        [Move, coord_buttons(X0-Y0, [B|Bs]), coord_buttons(X-Y, Bs)]>>
        (   foldl(move_coord_, Move, X0-Y0, X-Y),
            coord_button(X-Y, B)
        ),
        Moves,
        coord_buttons(1-1, Bs),
        coord_buttons(_, [])
    ),
    maplist(atom_number, S, Bs).

p2(S) :-
    phrase_from_file(sequence(move, "\n", Moves), 'input/d2.txt'),
    foldl(
        [Move, coord_buttons(X0-Y0, [B|Bs]), coord_buttons(X-Y, Bs)]>>
        (   foldl(p2_move_coord_, Move, X0-Y0, X-Y),
            p2_coord_button(X-Y, B)
        ),
        Moves,
        coord_buttons(0-2, Bs),
        coord_buttons(_, [])
    ),
    S = Bs.