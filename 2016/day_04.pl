:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

room(room(Name, ID, Checksum)) --> string(Name), "-", integer(ID), "[", string(Checksum), "]".

sum_room(Sum) -->
    tfilter(dif('-')),
    frequencies,
    predsort([D, K0-V0, K1-V1]>>compare(D, V1-K0, V0-K1)),
    pairs_keys,
    {Sum}/[Ks, _]>>n_list_split(5, Ks, Sum, _).

id_code_shifted(ID, X0, X) :-
    X0 in 0'- \/ 0'a..0'z,
    if_(X0 = 0'-,
        X = 0' ,
        X #= ((X0 - 97) + ID) mod 26 + 97).

p1(S) :-
    phrase_from_file(sequence(room, "\n", Rs0), 'input/d4.txt'),
    include([room(Name, _, Sum)]>>phrase(sum_room(Sum), Name), Rs0, Rs),
    maplist([room(_, ID, _), ID]>>true, Rs, IDs),
    sum_list(IDs, S).

p2(S) :-
    phrase_from_file(sequence(room, "\n", Rs0), 'input/d4.txt'),
    maplist([Room, Decrypted-ID]>>
            (Room = room(Name, ID, _),
             maplist(call_dcg((char_code, id_code_shifted(ID), [X, C]>>char_code(C, X))),
                     Name,
                     Decrypted)),
            Rs0,
            Rs),
    memberd("northpole object storage"-S, Rs).