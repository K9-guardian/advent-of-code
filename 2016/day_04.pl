:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

room(room(Name, ID, Checksum)) --> string(Name), "-", integer(ID), "[", string(Checksum), "]".

checksum(Sum) -->
    tfilter(dif('-')),
    frequencies,
    predsort([D, K0-V0, K1-V1]>>compare(D, V1-K0, V0-K1)),
    pairs_keys,
    {Sum}/[Ks, _]>>n_list_split(5, Ks, Sum, _).

id_code_shifted(ID, C0, C) :-
    char_code(C0, X0),
    if_(X0 = 0'-, X = 0' , X #= ((X0 - 97) + ID) mod 26 + 97),
    char_code(C, X).

p1(S) :-
    phrase_from_file(sequence(room, "\n", Rs0), 'input/d4.txt'),
    include([room(Name, _, Sum)]>>phrase(checksum(Sum), Name), Rs0, Rs),
    maplist([room(_, ID, _), ID]>>true, Rs, IDs),
    sum_list(IDs, S).

p2(S) :-
    phrase_from_file(sequence(room, "\n", Rs0), 'input/d4.txt'),
    maplist([room(Name, ID, _), Decrypted-ID]>>
            maplist(id_code_shifted(ID), Name, Decrypted),
            Rs0,
            Rs),
    memberd("northpole object storage"-S, Rs).