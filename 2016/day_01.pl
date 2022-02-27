:- use_module(library(dcg/basics)).

move(T-Amt) --> turn(T), integer(Amt).
turn('L') --> "L".
turn('R') --> "R".

file_parsed(File, Parsed) :-
    read_file_to_string(File, Input, []),
    split_string(Input, ",", " ", Vs),
    maplist([V, M]>>(string_chars(V, Cs), phrase(move(M), Cs)), Vs, Parsed).

dir_num('North', 0).
dir_num('East', 1).
dir_num('South', 2).
dir_num('West', 3).

turn_shift('L', -1).
turn_shift('R', 1).

dir_turn_(Dir0, Turn, Dir) :-
    dir_num(Dir0, Num0),
    turn_shift(Turn, Shift),
    Num #= (Num0 + Shift) mod 4,
    dir_num(Dir, Num).

dir_unit('North', 0-1).
dir_unit('East', 1-0).
dir_unit('South', 0-(-1)).
dir_unit('West', (-1)-0).

coord_dir_amt_(X0-Y0, Dir, Amt, X-Y) :-
    dir_unit(Dir, Xunit-Yunit),
    X #= X0 + Amt * Xunit,
    Y #= Y0 + Amt * Yunit.

coord_dir_amt_locs(X0-Y0, Dir, Amt, Locs) :-
    dir_unit(Dir, Xunit-Yunit),
    findall(Num, between(1, Amt, Num), Nums),
    maplist({X0, Y0, Xunit, Yunit}/[Num, X-Y]>>
            (   X #= X0 + Num * Xunit,
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
    file_parsed('input/d1.txt', Moves),
    foldl([Turn-Amt, coord_dir(X0-Y0, Dir0), coord_dir(X-Y, Dir)]>>
          (   dir_turn_(Dir0, Turn, Dir),
              coord_dir_amt_(X0-Y0, Dir, Amt, X-Y)
          ),
          Moves,
          coord_dir(0-0, 'North'),
          coord_dir(X-Y, _)),
    S #= X + Y.

p2(S) :-
    file_parsed('input/d1.txt', Moves),
    foldl([Turn-Amt, coord_dir_locs(X0-Y0, Dir0, Locs0), coord_dir_locs(X-Y, Dir, Locs)]>>
          (   dir_turn_(Dir0, Turn, Dir),
              coord_dir_amt_(X0-Y0, Dir, Amt, X-Y),
              coord_dir_amt_locs(X0-Y0, Dir, Amt, Locs_),
              phrase((Locs0, Locs_), Locs)
          ),
          Moves,
          coord_dir_locs(0-0, 'North', []),
          coord_dir_locs(_, _, Locs)),
    first_duplicate(X-Y, Locs),
    S #= X + Y.