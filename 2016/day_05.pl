:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(lib/double_quotes).
:- use_module(library(md5)).
:- use_module(lib/pio).
:- use_module(library(thread)).
:- use_module(library(yall)).

atom_codedigit(A, C) :-
    md5_hash(A, H, []),
    atom_concat('00000', S, H),
    atom_chars(S, [C|_]).

loop(ID, C) :-
    between(0, inf, X),
    atom_concat(ID, X, IDX),
    atom_codedigit(IDX, C).

p1(S) :-
    phrase_from_file(string(ID0), 'input/d5.txt'),
    atom_chars(ID, ID0),
    findnsols(8, C, loop(ID, C), S).