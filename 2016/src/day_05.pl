:- use_foreign_library(share/hash_helper).
:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

% data_char(D, C) :-
%     md5_hash(D, H, []),
%     atom_concat('00000', S, H),
%     atom_chars(S, [C|_]).

% p1(S) :-
%     phrase_from_file(string(ID), 'input/d5.txt'),
%     once(findnsols(8, Nonce, prefix_data_nonce("00000", ID, Nonce), Nonces)),
%     maplist(data_char of append(ID), Nonces, S).

% p2(S) :-
%     phrase_from_file(string(ID), 'input/d5.txt'),
%     append(ID, Nonce, Data),
%     functor(Passwd, p, 8),
%     n_data_passwd(0, Data-Nonce, Passwd),
%     Passwd =.. [_|S].

% :- use_module(lib/double_quotes).
% :- use_module(lib/pio).
% :- use_module(lib/util).
% :- use_module(library(dicts)).
% :- use_module(library(md5)).
% :- use_module(library(pairs)).

% data_chars(A, C0-C1) :-
%     md5_hash(A, H, []),
%     atom_concat('00000', S, H),
%     atom_chars(S, [C0, C1|_]).

% n_data_passwd(_, _, Passwd) :- ground(Passwd), !.
% n_data_passwd(N0, Data0-Nonce0, Passwd) :-
%     copy_term([Nonce0], Data0, [Nonce], Data),
%     number_chars(N0, Nonce0),
%     (   data_chars(Data0, Pos0-Char),
%         Pos = succ $ atom_number $ Pos0,
%         var(arg(Pos, Passwd, ~))
%     ->  setarg(Pos, Passwd, Char)
%     ;   true
%     ),
%     n_data_passwd(succ $ N0, Data-Nonce, Passwd).

% p1(S) :-
%     phrase_from_file(string(ID), 'input/d5.txt'),
%     append(ID, Nonce, Data),
%     once(findnsols(8, C, (between(1, inf, X), number_chars(X, Nonce), data_chars(Data, C-_)), Cs)),
%     S = Cs.

% p2(S) :-
%     phrase_from_file(string(ID), 'input/d5.txt'),
%     append(ID, Nonce, Data),
%     functor(Passwd, p, 8),
%     n_data_passwd(0, Data-Nonce, Passwd),
%     Passwd =.. [_|S].
