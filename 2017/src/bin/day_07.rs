use itertools::Itertools;
use regex::Regex;
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

type Graph = HashMap<Rc<String>, HashSet<Rc<String>>>;
type Weights = HashMap<Rc<String>, usize>;

struct Data(Graph, Weights);

impl std::str::FromStr for Data {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (mut graph, mut weights) = (HashMap::new(), HashMap::new());
        let re = Regex::new(r"(\w+) \((\d+)\)").unwrap();

        for l in s.lines() {
            let (caps, rc);

            match l.split_once(" -> ") {
                Some((left, right)) => {
                    caps = re.captures(left).unwrap();
                    rc = Rc::new(caps[1].to_string());

                    graph.insert(
                        Rc::clone(&rc),
                        right.split(", ").map(|s| Rc::new(s.to_string())).collect(),
                    );
                }
                None => {
                    caps = re.captures(l).unwrap();
                    rc = Rc::new(caps[1].to_string());
                }
            };

            weights.insert(Rc::clone(&rc), caps[2].parse().unwrap());
        }

        Ok(Data(graph, weights))
    }
}

fn p1(Data(graph, weights): &Data) -> Rc<String> {
    for prog in weights.keys() {
        if graph.values().all(|progs| !progs.contains(prog)) {
            return Rc::clone(prog);
        }
    }
    unreachable!()
}

fn solve(
    data @ Data(graph, weights): &Data,
    sums: &mut Weights,
    node: Rc<String>,
) -> Result<usize, String> {
    match graph.get(&node) {
        Some(neighbors) => {
            for nbr in neighbors {
                let n = solve(data, sums, Rc::clone(nbr))?;
                sums.insert(Rc::clone(nbr), n);
            }

            let ns = neighbors.iter().map(|nbr| sums[nbr]);
            if !ns.clone().all_equal() {
                // Imagine a programatic solve couldn't be me
                Err(neighbors
                    .iter()
                    .map(|nbr| format!("{:<7} {:^5} {:>5}\n", nbr, weights[nbr], sums[nbr]))
                    .collect())
            } else {
                let w = ns.sum::<usize>() + weights[&node];
                sums.insert(Rc::clone(&node), w);
                Ok(w)
            }
        }
        None => Ok(weights[&node]),
    }
}

fn p2(input: &Data) -> String {
    solve(input, &mut HashMap::new(), Rc::clone(&p1(input))).unwrap_err()
}

fn main() {
    let input = std::fs::read_to_string("input/d7.txt")
        .unwrap()
        .parse()
        .unwrap();
    dbg!(p1(&input));
    print!("{}", p2(&input));
}
