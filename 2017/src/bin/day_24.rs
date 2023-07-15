use std::collections::{HashMap, HashSet};

type Graph = HashMap<usize, HashSet<(usize, usize)>>;

fn p1(grp: &Graph) -> usize {
    let mut max_strength = 0;

    fn dfs(
        grp: &Graph,
        seen: HashSet<(usize, usize)>,
        (n, m): (usize, usize),
        strength: usize,
        max_strength: &mut usize,
    ) {
        *max_strength = strength.max(*max_strength);
        let mut new_seen = seen;
        new_seen.extend([(n, m), (m, n)]);

        for &(x, y) in &grp[&m] {
            if !new_seen.contains(&(x, y)) {
                dfs(
                    grp,
                    new_seen.clone(),
                    (x, y),
                    strength + x + y,
                    max_strength,
                );
            }
        }
    }

    for &start in grp[&0].iter() {
        dfs(
            grp,
            HashSet::new(),
            start,
            start.0 + start.1,
            &mut max_strength,
        );
    }

    max_strength
}

fn p2(grp: &Graph) -> usize {
    let mut longest_bridge = Vec::new();

    fn dfs(
        grp: &Graph,
        seen: HashSet<(usize, usize)>,
        (n, m): (usize, usize),
        bridge: Vec<(usize, usize)>,
        longest_bridge: &mut Vec<(usize, usize)>,
    ) {
        let mut new_seen = seen;
        new_seen.extend([(n, m), (m, n)]);

        let mut new_bridge = bridge;
        new_bridge.push((n, m));

        if (new_bridge.len() > longest_bridge.len())
            || (new_bridge.len() == longest_bridge.len()
                && new_bridge.iter().map(|(x, y)| x + y).sum::<usize>()
                    > longest_bridge.iter().map(|(x, y)| x + y).sum())
        {
            *longest_bridge = new_bridge.clone();
        }

        for &(x, y) in &grp[&m] {
            if !new_seen.contains(&(x, y)) {
                dfs(
                    grp,
                    new_seen.clone(),
                    (x, y),
                    new_bridge.clone(),
                    longest_bridge,
                );
            }
        }
    }

    for &start in grp[&0].iter() {
        dfs(grp, HashSet::new(), start, Vec::new(), &mut longest_bridge);
    }

    longest_bridge.iter().map(|(x, y)| x + y).sum()
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
    dbg!(p2(&grp));
}
