:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(yall)).

move(T-Amt) --> turn(T), integer(Amt).
turn(l) --> "L".
turn(r) --> "R".

file_parsed(File, Parsed) :-
    read_file_to_string(File, Input0, []),
    split_string(Input0, ",", " ", Input1),
    maplist([I, M]>>(string_chars(I, Cs), phrase(move(M), Cs)), Input1, Parsed).

dir_num_unit(north, 0, 0-1).
dir_num_unit(east, 1, 1-0).
dir_num_unit(south, 2, 0-(-1)).
dir_num_unit(west, 3, (-1)-0).

turn_shift(l, -1).
turn_shift(r, 1).

dir_turn_(Dir0, Turn, Dir) :-
    dir_num_unit(Dir0, Num0, _),
    turn_shift(Turn, Shift),
    Num #= (Num0 + Shift) mod 4,
    dir_num_unit(Dir, Num, _).

coord_dir_amt_(X0-Y0, Dir, Amt, X-Y) :-
    dir_num_unit(Dir, _, Xunit-Yunit),
    X #= X0 + Amt * Xunit,
    Y #= Y0 + Amt * Yunit.

coord_dir_amt_locs(X0-Y0, Dir, Amt, Locs) :-
    dir_num_unit(Dir, _, Xunit-Yunit),
    findall(Num, between(1, Amt, Num), Nums),
    maplist({X0, Y0, Xunit, Yunit}/[Num, X-Y]>>
            (   X #= X0 + Num * Xunit,
                Y #= Y0 + Num * Yunit
            ),
            Nums,
            Locs).

% Bad but performant predicate.
list_duplicate(Ls, E) :- list_duplicate_(Ls, set{}, E).

list_duplicate_([L|Ls], Set, E) :-
    (   L = Set.get(L) -> E = L
    ;   list_duplicate_(Ls, Set.put(L, L), E)
    ).

p1(S) :-
    file_parsed('input/d1.txt', Moves),
    foldl([Turn-Amt, coord_dir(X0-Y0, Dir0), coord_dir(X-Y, Dir)]>>
          (   dir_turn_(Dir0, Turn, Dir),
              coord_dir_amt_(X0-Y0, Dir, Amt, X-Y)
          ),
          Moves,
          coord_dir(0-0, north),
          coord_dir(X-Y, _)),
    S #= abs(X + Y).

p2(S) :-
    file_parsed('input/d1.txt', Moves),
    foldl([Turn-Amt, coord_dir_locs(X0-Y0, Dir0, Locs0), coord_dir_locs(X-Y, Dir, Locs)]>>
          (   dir_turn_(Dir0, Turn, Dir),
              coord_dir_amt_(X0-Y0, Dir, Amt, X-Y),
              coord_dir_amt_locs(X0-Y0, Dir, Amt, LocsP),
              append(LocsP, Locs, Locs0)
          ),
          Moves,
          coord_dir_locs(0-0, north, Locs),
          coord_dir_locs(_, _, [])),
    maplist(term_to_atom, [0-0|Locs], Atoms),
    list_duplicate(Atoms, Term),
    term_to_atom(X-Y, Term),
    S #= abs(X + Y).