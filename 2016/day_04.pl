:- use_module(lib/double_quotes).
:- use_module(lib/pio).
:- use_module(lib/util).

room(room(Name, ID, Checksum)) --> string(Name), "-", integer(ID), "[", string(Checksum), "]".

room_real(Checksum) -->
    tfilter(dif('-')),
    frequencies,
    assoc_to_list,
    predsort([D, K0-V0, K1-V1]>>compare(D, V1-K0, V0-K1)),
    pairs_keys,
    {Checksum}/[Ks, T]>>
    (   length(P, 5),
        append(P, _, Ks),
        if_(Checksum = P, T = true, T = false)
    ).

shift_char_(ID, C0, C) :-
    char_type(C0, ascii),
    (   char_type(C0, lower)
    ;   C0 = '-'
    ),
    if_(
        C0 = '-',
        C = ' ',
        (   char_code(C0, X0),
            X #= ((X0 - 97) + ID) mod 26 + 97,
            char_code(C, X)
        )
    ).

id_code_shifted(ID, X0, X) :-
    X0 in 0'- \/ 0'a..0'z,
    if_(X0 = 0'-,
        X = 0' ,
        X #= ((X0 - 97) + ID) mod 26 + 97
    ).

p1(S) :-
    phrase_from_file(sequence(room, "\n", Rs0), 'input/d4.txt'),
    tfilter([room(Name, _, Sum), T]>>call_dcg(room_real(Sum), Name, T), Rs0, Rs),
    maplist([room(_, ID, _), ID]>>true, Rs, IDs),
    sum_list(IDs, S).

p2(S) :-
    phrase_from_file(sequence(room, "\n", Rs0), 'input/d4.txt'),
    maplist(
        [Room, Decrypted-ID]>>
        (   Room = room(Name, ID, _),
            maplist(
                call_dcg((char_code,id_code_shifted(ID),[X, C]>>char_code(C, X))),
                Name,
                Decrypted
            )
        ),
        Rs0,
        Rs
    ),
    member("northpole object storage"-S, Rs).