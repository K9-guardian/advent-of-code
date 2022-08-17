:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).
:- use_module(library(lazy_lists)).

% We can simplify the computation by analyzing
% all possible combinations of left, center, and right.
left_center_right_trap('.', '.', '.') --> ".".
left_center_right_trap('.', '.', '^') --> "^".
left_center_right_trap('.', '^', '.') --> ".".
left_center_right_trap('.', '^', '^') --> "^".
left_center_right_trap('^', '.', '.') --> "^".
left_center_right_trap('^', '.', '^') --> ".".
left_center_right_trap('^', '^', '.') --> "^".
left_center_right_trap('^', '^', '^') --> ".".

% Account for the right wall being safe.
top_bottom_([A, B]) --> !, left_center_right_trap(A, B, '.').
top_bottom_([A, B, C|Ts]) --> left_center_right_trap(A, B, C), top_bottom_([B, C|Ts]).

% Account for the left wall being safe.
top_bottom_head(Ts, Bs, Bs) :- phrase(top_bottom_(['.'|Ts]), Bs).

grid(I, [I|G]) :- lazy_list(top_bottom_head, I, G).

% We require a special predicate to count the number of safe tiles
% as while the lists are lazy, prolog is not.
n_grid_safe_(0, [_|_]) --> !.
n_grid_safe_(N0, [R|Rs]) -->
    state(S0, S),
    { S1 #= length of tfilter(=('.')) $ R,
      S #= S0 + S1,
      N #= N0 - 1
    },
    n_grid_safe_(N, Rs).

p1(S) :-
    phrase_from_file(string(I), 'input/d18.txt'),
    grid(I, G),
    n_list_split(40, G, L, _),
    S = length of tfilter(=('.')) of append $ L.

p2(S) :-
    phrase_from_file(string(I), 'input/d18.txt'),
    grid(I, G),
    phrase(n_grid_safe_(400_000, G), [0], [S]).