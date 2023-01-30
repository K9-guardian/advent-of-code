use std::collections::{HashMap, HashSet};

const NUM_NODES: usize = 2000;

struct Graph(HashMap<usize, HashSet<usize>>);

impl std::str::FromStr for Graph {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut graph: HashMap<usize, HashSet<usize>> = HashMap::new();

        for l in s.lines() {
            let (left, right) = l.split_once(" <-> ").unwrap();
            graph.insert(
                left.parse().unwrap(),
                right.split(", ").map(|s| s.parse().unwrap()).collect(),
            );
        }

        Ok(Graph(graph))
    }
}

fn p1(Graph(graph): &Graph) -> usize {
    let mut stack = vec![0];
    let mut seen = [false; NUM_NODES];
    let mut ret = 0;

    while let Some(node) = stack.pop() {
        if !seen[node] {
            seen[node] = true;
            stack.extend(graph[&node].iter());
            ret += 1;
        }
    }

    ret
}

fn p2(Graph(graph): &Graph) -> usize {
    let mut stack = Vec::new();
    let mut seen = [false; NUM_NODES];
    let mut ret = 0;

    for node in 0..NUM_NODES {
        if !seen[node] {
            ret += 1;
        }

        stack.push(node);
        while let Some(node) = stack.pop() {
            if !seen[node] {
                seen[node] = true;
                stack.extend(graph[&node].iter());
            }
        }
    }

    ret
}

fn main() {
    let input = std::fs::read_to_string("input/d12.txt")
        .unwrap()
        .parse()
        .unwrap();
    dbg!(p1(&input));
    dbg!(p2(&input));
}
