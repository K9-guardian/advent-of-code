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

coord_dir_amt_(X-Y, Dir, Amt, X0-Y0) :-
    dir_unit(Dir, Xunit-Yunit),
    X0 #= X + Amt * Xunit,
    Y0 #= Y + Amt * Yunit.

p1(S) :-
    read_file_to_string('input/d1.txt', Input, []),
    split_string(Input, ",", " ", Vs),
    maplist([V, M]>>(string_chars(V, Cs), phrase(move(M), Cs)), Vs, Moves),
    foldl([Turn-Amt, coord_dir(X-Y, Dir), coord_dir(X0-Y0, Dir0)]>>
            (dir_turn_(Dir, Turn, Dir0),
             coord_dir_amt_(X-Y, Dir0, Amt, X0-Y0)),
          Moves,
          coord_dir(0-0, 'North'),
          coord_dir(X-Y, _)),
    S #= X + Y.