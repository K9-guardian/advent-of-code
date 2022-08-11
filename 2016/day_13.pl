:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

:- table num/1.

num(N) :- phrase_from_file(integer(N), 'input/d13.txt').

decimal_binary(N, Bs) :-
    zcompare(C, N, 1),
    comp_decimal_acc_binary(C, N, [], Bs).

comp_decimal_acc_binary(<, 0, Bs, [0|Bs]).
comp_decimal_acc_binary(=, 1, Bs, [1|Bs]).
comp_decimal_acc_binary(>, N, Bs0, Bs) :-
    divmod(N, 2, R, B),
    zcompare(C, R, 1),
    comp_decimal_acc_binary(C, R, [B|Bs0], Bs).

coord_type(X-Y, T) :-
    S0 #= X * X + 3 * X + 2 * X* Y + Y + Y * Y,
    S #= S0 + num(~),
    decimal_binary(S, Bs),
    L = length of tfilter(=(1)) $ Bs,
    if_(L mod 2 #= 0, T = open, T = wall).

coord0_coord(X0-Y, X-Y) :- X #= X0 + 1, X #>= 0, coord_type(X-Y, open).
coord0_coord(X0-Y, X-Y) :- X #= X0 - 1, X #>= 0, coord_type(X-Y, open).
coord0_coord(X-Y0, X-Y) :- Y #= Y0 + 1, Y #>= 0, coord_type(X-Y, open).
coord0_coord(X-Y0, X-Y) :- Y #= Y0 - 1, Y #>= 0, coord_type(X-Y, open).

queue_dists_([X-Y|Ps0], Ds0, Ds) :-
    (   X-Y = 31-39
    ->  Ds = Ds0
    ;   findall(I-J, coord0_coord(X-Y, I-J), Ps1),
        exclude({Ds0}/[X]>>get_assoc(X, Ds0, _), Ps1, Ps2),
        append(Ps0, Ps2, Ps),
        get_assoc(X-Y, Ds0, D0), succ(D0, D),
        foldl({D}/[X-Y, A0, A]>>put_assoc(X-Y, A0, D, A), Ps2, Ds0, Ds1),
        queue_dists_(Ps, Ds1, Ds)
    ).

p2_queue_dists_([X-Y|Ps0], Ds0, Ds) :-
    (   max_list of assoc_to_values $ Ds0 = 51
    ->  Ds = Ds0
    ;   findall(I-J, coord0_coord(X-Y, I-J), Ps1),
        exclude({Ds0}/[X]>>get_assoc(X, Ds0, _), Ps1, Ps2),
        append(Ps0, Ps2, Ps),
        get_assoc(X-Y, Ds0, D0), succ(D0, D),
        foldl({D}/[X-Y, A0, A]>>put_assoc(X-Y, A0, D, A), Ps2, Ds0, Ds1),
        p2_queue_dists_(Ps, Ds1, Ds)
    ).

p1(S) :-
    queue_dists_([1-1], list_to_assoc $ [1-1-0], Ds),
    get_assoc(31-39, Ds, S).

p2(S) :-
    p2_queue_dists_([1-1], list_to_assoc $ [1-1-0], Ds),
    assoc_to_list(Ds, Ls0),
    include([_-_-D]>>(D #=< 50), Ls0, Ls),
    length(Ls, S).