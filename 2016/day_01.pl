:- use_module(lib/dcg).
:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

turn(l) --> "L".
turn(r) --> "R".
move(T-Amt) --> turn(T), integer(Amt).

dir_unit(0, 0-1). % North
dir_unit(1, 1-0). % East
dir_unit(2, 0-(-1)). % South
dir_unit(3, (-1)-0). % West

turn_shift(l, -1).
turn_shift(r, 1).

dir_turn_(Dir0, Turn, Dir) :- Dir #= (Dir0 + (turn_shift $ Turn)) mod 4.

coord_dir_amt_(X0-Y0, Dir, Amt, X-Y) :-
    dir_unit(Dir, I-J),
    X #= X0 + Amt * I,
    Y #= Y0 + Amt * J.

coord_dir_amt_locs(X0-Y0, Dir, Amt, Locs) :-
    dir_unit(Dir, I-J),
    maplist(
        {X0, Y0, I, J}/[Num, X-Y]>>
        (   X #= X0 + Num * I,
            Y #= Y0 + Num * J
        ),
        numlist(1, Amt, ~),
        Locs
    ).

p1(S) :-
    phrase_from_file(sequence(move, ", ", Moves), 'input/d1.txt'),
    foldl(
        [Turn-Amt, coord_dir(X0-Y0, Dir0), coord_dir(X-Y, Dir)]>>
        (   dir_turn_(Dir0, Turn, Dir),
            coord_dir_amt_(X0-Y0, Dir, Amt, X-Y)
        ),
        Moves,
        coord_dir(0-0, 0),
        coord_dir(X-Y, _)
    ),
    S #= abs(X + Y).

p2(S) :-
    phrase_from_file(sequence(move, ", ", Moves), 'input/d1.txt'),
    foldl(
        [Turn-Amt, coord_dir_locs(X0-Y0, Dir0, Locs0), coord_dir_locs(X-Y, Dir, Locs)]>>
        (   dir_turn_(Dir0, Turn, Dir),
            coord_dir_amt_(X0-Y0, Dir, Amt, X-Y),
            coord_dir_amt_locs(X0-Y0, Dir, Amt, LocsP),
            append(LocsP, Locs, Locs0)
        ),
        Moves,
        coord_dir_locs(0-0, 0, Locs),
        coord_dir_locs(_, _, [])
    ),
    list_firstdup([0-0|Locs], X-Y),
    S #= abs(X + Y).