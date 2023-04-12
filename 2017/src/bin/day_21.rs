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
    // Strategy: We see that every 3x3 turns into a 9x9
    // We can split these 9x9s into 9 3x3s and run them independently
    // We want to precompute all the possible 3x3s (we have 2^9) and find what 9x9s they turn into
}

fn main() {
    let rules: Vec<Rule> = std::fs::read_to_string("input/d21.txt")
        .unwrap()
        .lines()
        .map(|s| s.parse().unwrap())
        .collect();
    dbg!(p1(&rules));
}
