use std::collections::{HashMap, HashSet};

type Graph = HashMap<usize, HashSet<(usize, usize)>>;

fn p1(grp: &Graph) -> usize {
    let mut max_strength = 0;

    // TODO: stop the randomness
    fn dfs(
        grp: &Graph,
        seen: &mut HashSet<(usize, usize)>,
        (n, m): (usize, usize),
        strength: usize,
        max_strength: &mut usize,
    ) {
        *max_strength = strength.max(*max_strength);
        println!("{n}/{m} {strength} {max_strength}");
        seen.insert((n, m));

        for &(x, y) in &grp[&m] {
            if !seen.contains(&(x, y)) && !seen.contains(&(y, x)) {
                dfs(grp, seen, (x, y), strength + x + y, max_strength);
            }
        }
    }

    for &start in grp[&0].iter() {
        dfs(
            grp,
            &mut HashSet::new(),
            start,
            start.0 + start.1,
            &mut max_strength,
        );
        println!();
    }

    max_strength
}

fn components_to_graph(cmps: &[(usize, usize)]) -> Graph {
    let mut grp: Graph = HashMap::new();

    for &(x, y) in cmps {
        grp.entry(x)
            .and_modify(|s| {
                s.insert((x, y));
            })
            .or_insert(HashSet::from([(x, y)]));
        grp.entry(y)
            .and_modify(|s| {
                s.insert((y, x));
            })
            .or_insert(HashSet::from([(y, x)]));
    }

    grp
}

fn main() {
    // let cmps: Vec<_> = std::fs::read_to_string("example.txt")
    let cmps: Vec<_> = std::fs::read_to_string("input/d24.txt")
        .unwrap()
        .lines()
        .map(|s| {
            let (left, right) = s.split_once('/').unwrap();
            (
                left.parse::<usize>().unwrap(),
                right.parse::<usize>().unwrap(),
            )
        })
        .collect();
    let grp = components_to_graph(&cmps);
    dbg!(p1(&grp));
}
