fn p1(step: usize) -> usize {
    let mut idx = 0;
    let mut buf = vec![0];

    for i in 1..=2017 {
        idx = (idx + step + 1) % buf.len();
        buf.insert(idx, i);
    }

    buf[(idx + 1) % buf.len()]
}

fn p2(step: usize) -> usize {
    let mut idx = 0;
    let mut buf = Vec::with_capacity(50_000_001);
    buf.push(0);

    for i in 1..=50_000_000 {
        idx = (idx + step + 1) % buf.len();
        buf.insert(idx, i);
    }

    buf[(idx + 1) % buf.len()]
}

fn main() {
    let input: usize = std::fs::read_to_string("input/d17.txt")
        .unwrap()
        .parse()
        .unwrap();
    dbg!(p1(input));
    dbg!(p2(input));
}
