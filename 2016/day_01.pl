:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).
:- use_module(library(yall)).

move(T-Amt) --> turn(T), integer(Amt).
turn('L') --> "L".
turn('R') --> "R".

dir_turn_('North', 'L', 'West').
dir_turn_('North', 'R', 'East').
dir_turn_('South', 'L', 'East').
dir_turn_('South', 'R', 'West').
dir_turn_('East', 'L', 'North').
dir_turn_('East', 'R', 'South').
dir_turn_('West', 'L', 'South').
dir_turn_('West', 'R', 'North').

dir_unit('North', 0-1).
dir_unit('South', 0-(-1)).
dir_unit('East', 1-0).
dir_unit('West', (-1)-0).

coord_dir_amt_(X0-Y0, Dir, Amt, X-Y) :-
    dir_unit(Dir, Xunit-Yunit),
    X #= X0 + Amt * Xunit,
    Y #= Y0 + Amt * Yunit.

coord_dir_amt_locs(X0-Y0, Dir, Amt, Locs) :-
    dir_unit(Dir, Xunit-Yunit),
    findall(Num, between(1, Amt, Num), Nums),
    maplist(
        {X0, Y0, Xunit, Yunit}/[Num, X-Y]>>(
            X #= X0 + Num * Xunit,
            Y #= Y0 + Num * Yunit
        ),
        Nums,
        Locs).

first_duplicate(E, [E|Ls]) :-
    member(E, Ls).
first_duplicate(E, [N|Ls]) :-
    maplist(dif(N), Ls),
    first_duplicate(E, Ls).

p1(S) :-
    read_file_to_string('input/d1.txt', Input, []),
    split_string(Input, ",", " ", Vs),
    maplist([V, M]>>(string_chars(V, Cs), phrase(move(M), Cs)), Vs, Moves),
    foldl(
        [Turn-Amt, coord_dir(X0-Y0, Dir0), coord_dir(X-Y, Dir)]>>(
            dir_turn_(Dir0, Turn, Dir),
            coord_dir_amt_(X0-Y0, Dir, Amt, X-Y)
        ),
        Moves,
        coord_dir(0-0, 'North'),
        coord_dir(X-Y, _)),
    S #= X + Y.

p2(S) :-
    read_file_to_string('input/d1.txt', Input, []),
    split_string(Input, ",", " ", Vs),
    maplist([V, M]>>(string_chars(V, Cs), phrase(move(M), Cs)), Vs, Moves),
    foldl(
        [Turn-Amt, coord_dir(X0-Y0, Dir0)-Locs0, coord_dir(X-Y, Dir)-Locs]>>(
            dir_turn_(Dir0, Turn, Dir),
            coord_dir_amt_(X0-Y0, Dir, Amt, X-Y),
            coord_dir_amt_locs(X0-Y0, Dir, Amt, Locs1),
            append(Locs0, Locs1, Locs)
        ),
        Moves,
        coord_dir(0-0, 'North')-[],
        _-Locs),
    first_duplicate(X-Y, Locs),
    S #= X + Y.