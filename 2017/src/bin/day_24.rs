fn p1(cmps: &[(usize, usize)]) -> usize {
    let bridge = Vec::new(); // should be a cons list i think

    // TODO: Choose a component
    // find another that can bridge (precompute num->bridge)
    // wait... is this just a dfs
    // max distance path in a graph
    // i think we just

    cmps.sort();
    0
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
    dbg!(p1(&cmps));
}
