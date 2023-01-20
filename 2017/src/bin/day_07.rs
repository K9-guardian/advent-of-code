use itertools::Itertools;
use regex::Regex;
use std::collections::{HashMap, HashSet};

type Graph = HashMap<String, HashSet<String>>;
type Weights = HashMap<String, usize>;

struct Data(Graph, Weights);

impl std::str::FromStr for Data {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (mut graph, mut weights) = (HashMap::new(), HashMap::new());
        let re = Regex::new(r"(\w+) \((\d+)\)").unwrap();

        for l in s.lines() {
            let caps;

            match l.split_once(" -> ") {
                Some((left, right)) => {
                    caps = re.captures(left).unwrap();

                    graph.insert(
                        String::from(&caps[1]),
                        right.split(", ").map(String::from).collect(),
                    );
                }
                None => caps = re.captures(l).unwrap(),
            };

            weights.insert(String::from(&caps[1]), caps[2].parse().unwrap());
        }

        Ok(Data(graph, weights))
    }
}

fn p1(Data(graph, weights): &Data) -> &str {
    for prog in weights.keys() {
        if graph.values().all(|progs| !progs.contains(prog)) {
            return prog;
        }
    }
    unreachable!()
}

fn p2(input: &Data) -> String {
    let Data(graph, weights) = input;
    let mut sums: Weights = HashMap::new();

    fn solve(
        graph: &Graph,
        weights: &Weights,
        sums: &mut Weights,
        node: &str,
    ) -> Result<usize, String> {
        match graph.get(node) {
            Some(neighbors) => {
                for nbr in neighbors {
                    let n = solve(graph, weights, sums, &nbr)?;
                    sums.insert(String::from(nbr), n);
                }

                if !neighbors.iter().map(|nbr| sums[nbr]).all_equal() {
                    // Imagine a programatic solve couldn't be me
                    Err(neighbors
                        .iter()
                        .map(|nbr| format!("{:<7} {:^5} {:>5}\n", nbr, weights[nbr], sums[nbr]))
                        .collect())
                } else {
                    let w = neighbors.iter().map(|nbr| sums[nbr]).sum::<usize>() + weights[node];
                    sums.insert(String::from(node), w);
                    Ok(w)
                }
            }
            None => Ok(weights[node]),
        }
    }

    solve(&graph, &weights, &mut sums, &p1(input)).unwrap_err()
}

fn main() {
    let input = std::fs::read_to_string("input/d7.txt")
        .unwrap()
        .parse()
        .unwrap();
    dbg!(p1(&input));
    print!("{}", p2(&input));
}
