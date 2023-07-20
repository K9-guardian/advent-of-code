use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
struct Action {
    write: usize,
    mov: i32,
    cont: usize,
}

#[derive(Debug, Clone, Copy)]
struct TuringMachine {
    starting_state: usize,
    jmp_table: [[Action; 2]; 6],
    steps: usize,
}

fn p1(
    &TuringMachine {
        starting_state,
        jmp_table,
        steps,
    }: &TuringMachine,
) -> usize {
    let (mut cursor, mut tape, mut state) = (0, HashMap::<i32, usize>::new(), starting_state);

    for _ in 0..steps {
        let val = *tape.get(&cursor).unwrap_or(&0);
        let act = jmp_table[state][val];

        tape.insert(cursor, act.write);
        cursor += act.mov;
        state = act.cont;
    }

    tape.values().sum()
}

fn main() {
    // parsing hard
    let input: TuringMachine = TuringMachine {
        starting_state: 0,
        jmp_table: [
            [
                Action {
                    write: 1,
                    mov: 1,
                    cont: 1,
                },
                Action {
                    write: 0,
                    mov: -1,
                    cont: 5,
                },
            ],
            [
                Action {
                    write: 0,
                    mov: 1,
                    cont: 2,
                },
                Action {
                    write: 0,
                    mov: 1,
                    cont: 3,
                },
            ],
            [
                Action {
                    write: 1,
                    mov: -1,
                    cont: 3,
                },
                Action {
                    write: 1,
                    mov: 1,
                    cont: 4,
                },
            ],
            [
                Action {
                    write: 0,
                    mov: -1,
                    cont: 4,
                },
                Action {
                    write: 0,
                    mov: -1,
                    cont: 3,
                },
            ],
            [
                Action {
                    write: 0,
                    mov: 1,
                    cont: 0,
                },
                Action {
                    write: 1,
                    mov: 1,
                    cont: 2,
                },
            ],
            [
                Action {
                    write: 1,
                    mov: -1,
                    cont: 0,
                },
                Action {
                    write: 1,
                    mov: 1,
                    cont: 0,
                },
            ],
        ],
        steps: 12794428,
    };
    dbg!(p1(&input));
}
