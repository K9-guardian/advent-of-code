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
            vec![b, c, d, a],
            vec![c, d, a, b],
            vec![d, a, b, c],
            vec![b, a, d, c],
            vec![a, d, c, b],
            vec![d, c, b, a],
            vec![c, b, a, d],
        ],
        [a, b, c, d, e, f, g, h, i] => [
            vec![a, b, c, d, e, f, g, h, i],
            vec![c, f, i, h, g, d, a, b, e],
            vec![i, h, g, d, a, b, c, f, e],
            vec![g, d, a, b, c, f, i, h, e],
            vec![c, b, a, f, e, d, i, h, g],
            vec![a, d, g, h, i, f, c, b, e],
            vec![g, h, i, f, c, b, a, d, e],
            vec![i, f, c, b, a, d, g, h, e],
        ],
        _ => unreachable!(),
    }
}

fn p1(_input: &[Rule]) -> usize {
    let (state, mut size) = (
        vec![false, true, false, false, false, true, true, true, true],
        3,
    );

    // Split state into vectors of right size
    // Find pattern that matches a rotation
    // Execute pattern
    // Move back to normal
    // TODO: i suck at this
    for _ in 0..5 {
        if size % 2 == 0 {
            size *= 3 / 2;
        } else {
            size *= 4 / 3;
        }
    }

    state.iter().filter(|&&b| b).count()
}

fn main() {
    let input: Vec<Rule> = std::fs::read_to_string("input/d20.txt")
        .unwrap()
        .lines()
        .map(|s| s.parse().unwrap())
        .collect();
    dbg!(p1(&input));
}
