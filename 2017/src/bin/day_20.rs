use std::collections::HashMap;

use itertools::Itertools;

#[derive(Debug, Clone, Copy)]
struct Particle {
    pos: (i64, i64, i64),
    vel: (i64, i64, i64),
    acc: (i64, i64, i64),
}

impl std::str::FromStr for Particle {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn parse_coordinates(input: &str) -> (i64, i64, i64) {
            let v: Vec<_> = input[3..(input.len() - 1)]
                .split(',')
                .map(|s| s.parse().unwrap())
                .collect();
            (v[0], v[1], v[2])
        }

        let vals: Vec<_> = s.split(", ").collect();

        Ok(Particle {
            pos: parse_coordinates(vals[0]),
            vel: parse_coordinates(vals[1]),
            acc: parse_coordinates(vals[2]),
        })
    }
}

fn simulate(state: &mut [Particle]) {
    for p in state {
        p.vel.0 += p.acc.0;
        p.vel.1 += p.acc.1;
        p.vel.2 += p.acc.2;

        p.pos.0 += p.vel.0;
        p.pos.1 += p.vel.1;
        p.pos.2 += p.vel.2;
    }
}

fn collision_simulate(
    state: &mut [Option<Particle>],
    positions: &mut HashMap<(i64, i64, i64), Vec<usize>>,
) {
    positions.clear();

    for (i, particle) in state.iter_mut().enumerate() {
        if let Some(p) = particle {
            p.vel.0 += p.acc.0;
            p.vel.1 += p.acc.1;
            p.vel.2 += p.acc.2;

            p.pos.0 += p.vel.0;
            p.pos.1 += p.vel.1;
            p.pos.2 += p.vel.2;

            positions
                .entry(p.pos)
                .and_modify(|v| v.push(i))
                .or_insert_with(|| vec![i]);
        }
    }

    for ps in positions.values_mut() {
        if ps.len() > 1 {
            ps.iter().for_each(|&p| state[p] = None);
        }
    }
}

fn p1(input: &mut [Particle]) -> usize {
    for _ in 0..1000 {
        simulate(input);
    }

    input
        .iter()
        .position_min_by(|x, y| {
            (x.pos.0.abs() + x.pos.1.abs() + x.pos.2.abs())
                .cmp(&(y.pos.0.abs() + y.pos.1.abs() + y.pos.2.abs()))
        })
        .unwrap()
}

fn p2(input: &[Particle]) -> usize {
    let mut particles: Vec<_> = input.iter().copied().map(Option::Some).collect();
    let mut positions = HashMap::new();

    for _ in 0..1000 {
        collision_simulate(&mut particles, &mut positions);
    }

    particles.iter().filter_map(|&e| e).count()
}

fn main() {
    let input: Vec<Particle> = std::fs::read_to_string("input/d20.txt")
        .unwrap()
        .lines()
        .map(|s| s.parse().unwrap())
        .collect();
    dbg!(p1(&mut input.clone()));
    dbg!(p2(&input));
}
