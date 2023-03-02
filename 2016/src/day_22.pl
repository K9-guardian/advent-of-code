:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

line(nil) --> "root@ebhq-gridcenter# df -h".
line(nil) --> "Filesystem              Size  Used  Avail  Use%".
line(node_stats(X-Y, U-A)) -->
    "/dev/grid/node-x", integer(X), "-y", integer(Y),
    whites, nonblanks(_),
    whites, integer(U), "T",
    whites, integer(A), "T",
    whites, nonblanks(_).

nodes_viable_pair(Nodes, node_stats(P0, U-_)-node_stats(P1, _-A)) :-
    chain([1, U, A], #=<),
    foldl(select, [node_stats(P0, U-_), node_stats(P1, _-A)], Nodes, _).

node_pretty(0-_, '_').
node_pretty(U-A, P) :-
    T #= U + A,
    if_(T #>= 500, P = '#', P = '.').

p1(S) :-
    phrase_from_file(sequence(line, "\n", [_, _|Nodes]), 'input/d22.txt'),
    findall(P, nodes_viable_pair(Nodes, P), L),
    length(L, S).

% Near impossible to write a code solution to this problem.
% Instead, we look at patterns in the input.
% We see that there's one empty node, and in fact all the solutions from
% part 1 use this empty node as the sink.
% Thus, we can reduce this problem to a grid with an empty node, and barriers
% for the large nodes (larger than 500T) that can't fit in the empty node.

% Our empty node is at 13-23.
% We have 63 moves to move our empty tile to the second to top right.
% From there, we use 1 move to move data left, and 4 moves to shuffle the empty tile.
% This gives us 63 + (5 * 37) + 1 = 249 moves.
p2(S) :-
    phrase_from_file(sequence(line, "\n", [_, _|Nodes]), 'input/d22.txt'),
    maplist({G}/[node_stats(X-Y, U-A)]>>(nth0(Y, G, L), nth0(X, L, U-A)), Nodes),
    maplist([L]>>append(L, [], L), G),
    maplist([L0]>>(maplist(node_pretty, L0, L), format('~s~n', [L])), G),
    S = G.
