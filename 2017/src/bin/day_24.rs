fn p1(cmps: &mut [(usize, usize)]) -> usize {
    cmps.sort();
    0
}

fn main() {
    let mut cmps: Vec<_> = std::fs::read_to_string("input/d24.txt")
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
    dbg!(p1(&mut cmps.clone()));
}
