:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

line(nil) --> "root@ebhq-gridcenter# df -h".
line(nil) --> "Filesystem              Size  Used  Avail  Use%".
line(node_size_used_avail_percent(X-Y, S, U, A, P)) -->
    "/dev/grid/node-x", integer(X), "-y", integer(Y), whites,
    integer(S), "T", whites,
    integer(U), "T", whites,
    integer(A), "T", whites,
    integer(P), "%".

nodes_viable_pair(Nodes,
                  node_size_used_avail_percent(_, _, U, _, _)-
                  node_size_used_avail_percent(_, _, _, A, _)) :-
    U #> 0, U #=< A,
    foldl(select,
          [node_size_used_avail_percent(_, _, U, _, _),
           node_size_used_avail_percent(_, _, _, A, _)],
          Nodes,
          _).

p1(S) :-
    phrase_from_file(sequence(line, "\n", [_, _|Nodes]), 'input/d22.txt'),
    findall(P, nodes_viable_pair(Nodes, P), Ps),
    length(Ps, S).