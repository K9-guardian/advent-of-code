use itertools::Itertools;

type Pattern = Vec<bool>;

struct Rule(Pattern, Pattern);

impl std::str::FromStr for Rule {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn parse_pattern(s: &str) -> Pattern {
            s.chars()
                .filter_map(|c| match c {
                    '#' => Some(true),
                    '.' => Some(false),
                    _ => None,
                })
                .collect()
        }

        let (left, right) = s.split_once(" => ").unwrap();
        Ok(Rule(parse_pattern(left), parse_pattern(right)))
    }
}

// Hardcode flips and rotations
fn pattern_variations(p: &Pattern) -> [Pattern; 8] {
    match p[..] {
        [a, b, c, d] => [
            vec![a, b, c, d],
            vec![b, d, a, c],
            vec![d, c, b, a],
            vec![c, a, d, b],
            vec![b, a, d, c],
            vec![a, c, b, d],
            vec![c, d, a, b],
            vec![d, b, c, a],
        ],
        [a, b, c, d, e, f, g, h, i] => [
            vec![a, b, c, d, e, f, g, h, i],
            vec![c, f, i, b, e, h, a, d, g],
            vec![i, h, g, f, e, d, c, b, a],
            vec![g, d, a, h, e, b, i, f, c],
            vec![c, b, a, f, e, d, i, h, g],
            vec![a, d, g, b, e, h, c, f, i],
            vec![g, h, i, d, e, f, a, b, c],
            vec![i, f, c, h, e, b, g, d, a],
        ],
        _ => unreachable!(),
    }
}

fn p1(rules: &[Rule]) -> usize {
    // We need to go faster!
    let state = vec![
        vec![false, true, false],
        vec![false, false, true],
        vec![true, true, true],
    ];

    (0..3000)
        .fold(state, |state, _| {
            // for row in &state {
            //     for b in row {
            //         match b {
            //             true => print!("#"),
            //             false => print!("."),
            //         }
            //     }
            //     println!();
            // }
            // println!();

            let len = state.len();
            let (mut new_state, new_len): (Vec<Vec<bool>>, usize);
            let mut boxes = Vec::new();

            if len % 2 == 0 {
                new_len = len * 3 / 2;
                for i in 0..(len / 2) {
                    for j in 0..(len / 2) {
                        boxes.push(vec![
                            state[i][j],
                            state[i][j + 1],
                            state[i + 1][j],
                            state[i + 1][j + 1],
                        ])
                    }
                }

                new_state = vec![vec![false; new_len]; new_len];

                for (i, row) in boxes
                    .iter()
                    .map(|b| {
                        pattern_variations(b)
                            .iter()
                            .find_map(|p| {
                                rules.iter().find_map(
                                    |Rule(l, r)| if p == l { Some(r.clone()) } else { None },
                                )
                            })
                            .unwrap()
                    })
                    .chunks(len)
                    .into_iter()
                    .enumerate()
                {
                    for (j, sq) in row.enumerate() {
                        new_state[i][j] = sq[0];
                        new_state[i][j + 1] = sq[1];
                        new_state[i + 1][j] = sq[2];
                        new_state[i + 1][j + 1] = sq[3];
                    }
                }
            } else {
                new_len = len * 4 / 3;
                for i in 0..(len / 3) {
                    for j in 0..(len / 3) {
                        boxes.push(vec![
                            state[i][j],
                            state[i][j + 1],
                            state[i][j + 2],
                            state[i + 1][j],
                            state[i + 1][j + 1],
                            state[i + 1][j + 2],
                            state[i + 2][j],
                            state[i + 2][j + 1],
                            state[i + 2][j + 2],
                        ])
                    }
                }

                new_state = vec![vec![false; new_len]; new_len];

                for (i, row) in boxes
                    .iter()
                    .map(|b| {
                        pattern_variations(b)
                            .iter()
                            .find_map(|p| {
                                rules.iter().find_map(
                                    |Rule(l, r)| if p == l { Some(r.clone()) } else { None },
                                )
                            })
                            .unwrap()
                    })
                    .chunks(len)
                    .into_iter()
                    .enumerate()
                {
                    for (j, sq) in row.enumerate() {
                        new_state[i][j] = sq[0];
                        new_state[i][j + 1] = sq[1];
                        new_state[i][j + 2] = sq[2];
                        new_state[i + 1][j] = sq[3];
                        new_state[i + 1][j + 1] = sq[4];
                        new_state[i + 1][j + 2] = sq[5];
                        new_state[i + 2][j] = sq[6];
                        new_state[i + 2][j + 1] = sq[7];
                        new_state[i + 2][j + 2] = sq[8];
                    }
                }
            }

            new_state
        })
        .iter()
        .flatten()
        .filter(|&&b| b)
        .count()
}

fn main() {
    let rules: Vec<Rule> = std::fs::read_to_string("input/d21.txt")
        .unwrap()
        .lines()
        .map(|s| s.parse().unwrap())
        .collect();
    dbg!(p1(&rules));
}
