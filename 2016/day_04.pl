:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(library(pairs)).
:- use_module(library(yall)).

room(room(Name, ID, Checksum)) --> string(Name), "-", integer(ID), "[", string(Checksum), "]".

frequencies(Es, Freqs) :-
    foldl([E, Fs0, Fs]>>(
              (   get_dict(E, Fs0, X0) -> succ(X0, X)
              ;   X = 1
              ),
              put_dict(E, Fs0, X, Fs)
          ),
          Es,
          freqs{},
          Freqs).

room_real(room(Name, _, Checksum), T) :-
    tfilter(dif('-'), Name, Letters),
    msort(Letters, Letters_Sorted),
    frequencies(Letters_Sorted, Freqs),
    dict_pairs(Freqs, _, Pairs),
    sort(2, @>=, Pairs, Sorted),
    pairs_keys(Sorted, Ks),
    length(KsP, 5),
    append(KsP, _, Ks),
    if_(Checksum = KsP, T = true, T = false).

shift(ID, C0, C) :-
    if_(C0 = '-',
        C = ' ',
        (char_code(C0, X0),
         X #= ((X0 - 97) + ID) mod 26 + 97,
         char_code(C, X))).

room_decrypted(room(Name, ID, _), Decrypted) :-
    maplist(shift(ID), Name, Decrypted).

p1(S) :-
    phrase_from_file(sequence(room, "\n", Rs0), 'input/d4.txt'),
    tfilter(room_real, Rs0, Rs),
    maplist([room(_, ID, _), ID]>>true, Rs, IDs),
    sum_list(IDs, S).

p2(S) :-
    phrase_from_file(sequence(room, "\n", Rs0), 'input/d4.txt'),
    maplist([Room, Decrypted-ID]>>
            (Room = room(_, ID, _),
             room_decrypted(Room, Decrypted)),
             Rs0,
             Rs),
    member("northpole object storage"-S, Rs).