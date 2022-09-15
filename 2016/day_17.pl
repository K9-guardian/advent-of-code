:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/queue).
:- use_module(lib/util).
:- use_module(library(md5)).
:- use_module(library(pairs)).

:- table input/1.

input(I) :- phrase_from_file(string(I), 'input/d17.txt').

move_coord0_coord('U', X-1, X-0).
move_coord0_coord('U', X-2, X-1).
move_coord0_coord('U', X-3, X-2).
move_coord0_coord('D', X-0, X-1).
move_coord0_coord('D', X-1, X-2).
move_coord0_coord('D', X-2, X-3).
move_coord0_coord('L', 1-Y, 0-Y).
move_coord0_coord('L', 2-Y, 1-Y).
move_coord0_coord('L', 3-Y, 2-Y).
move_coord0_coord('R', 0-Y, 1-Y).
move_coord0_coord('R', 1-Y, 2-Y).
move_coord0_coord('R', 2-Y, 3-Y).

queue_path([X0-Y0-P0|XYPs0], P) :-
    (   X0-Y0 = 3-3
    ->  P = P0
    ;   append(input(~), P0, D),
        md5_hash(D, H, []),
        atom_chars(H, [Up, Down, Left, Right|_]),
        Dirs = ['U'-Up, 'D'-Down, 'L'-Left, 'R'-Right],
        phrase((tfilter([_-V, T]>>memberd_t(V, "bcdef", T)),
                pairs_keys,
                convlist({X0-Y0, P0}/[M, X-Y-P1]>>
                (move_coord0_coord(M, X0-Y0, X-Y), append(P0, [M], P1)))),
               Dirs,
               XYPs1),
        append(XYPs0, XYPs1, XYPs),
        queue_path(XYPs, P)
    ).

stack_maxpath_([], M, M).
stack_maxpath_([X0-Y0-P0|XYPs0], M0, M) :-
    (   X0-Y0 = 3-3
    ->  M1 #= max(M0, length $ P0),
        stack_maxpath_(XYPs0, M1, M)
    ;   append(input(~), P0, D),
        md5_hash(D, H, []),
        atom_chars(H, [Up, Down, Left, Right|_]),
        Dirs = ['U'-Up, 'D'-Down, 'L'-Left, 'R'-Right],
        phrase((tfilter([_-V, T]>>memberd_t(V, "bcdef", T)),
                pairs_keys,
                convlist({X0-Y0, P0}/[M, X-Y-P1]>>
                (move_coord0_coord(M, X0-Y0, X-Y), append(P0, [M], P1)))),
               Dirs,
               XYPs1),
        append(XYPs1, XYPs0, XYPs),
        stack_maxpath_(XYPs, M0, M)
    ).

p1(S) :- queue_path([0-0-""], S).

p2(S) :- stack_maxpath_([0-0-""], 0, S).
