const GEN_A_FACTOR: usize = 16807;
const GEN_B_FACTOR: usize = 48271;
const MOD: usize = 2147483647;

fn p1(input: &[usize]) -> usize {
    let (mut a, mut b) = (input[0], input[1]);

    (0..40_000_000)
        .filter(|_| {
            a = (a * GEN_A_FACTOR) % MOD;
            b = (b * GEN_B_FACTOR) % MOD;

            (a as u16) == (b as u16)
        })
        .count()
}

fn p2(input: &[usize]) -> usize {
    let (mut a, mut b) = (input[0], input[1]);
    let mut ret = 0;

    for _ in 0..5_000_000 {
        while a % 4 != 0 {
            a = (a * GEN_A_FACTOR) % MOD;
        }
        while b % 8 != 0 {
            b = (b * GEN_B_FACTOR) % MOD;
        }

        if (a as u16) == (b as u16) {
            ret += 1;
        }

        a = (a * GEN_A_FACTOR) % MOD;
        b = (b * GEN_B_FACTOR) % MOD;
    }

    ret
}

fn main() {
    let input: Vec<_> = std::fs::read_to_string("input/d15.txt")
        .unwrap()
        .lines()
        .map(|s| {
            s.split_whitespace()
                .last()
                .unwrap()
                .parse::<usize>()
                .unwrap()
        })
        .collect();
    dbg!(p1(&input));
    dbg!(p2(&input));
}
