fn p1(input: &str) -> usize {
    const SIZE: usize = 256;
    let mut circle: Vec<_> = (0..SIZE).collect();
    let (mut pos, mut skip) = (0, 0);

    for n in input.split(',').map(|s| s.parse::<usize>().unwrap()) {
        let indices = (pos..(pos + n)).map(|i| i % SIZE);
        let reversed: Vec<_> = indices.clone().map(|i| circle[i]).rev().collect();
        indices.zip(reversed).for_each(|(i, n)| circle[i] = n);

        pos = (pos + n + skip) % SIZE;
        skip += 1;
    }

    circle[0] * circle[1]
}

fn p2(input: &str) -> usize {
    let s: Vec<_> = input.bytes().chain(vec![17, 31, 73, 47, 23]).collect();
    dbg!(s);
    0
}

fn main() {
    let input = std::fs::read_to_string("input/d10.txt").unwrap();
    dbg!(p1(&input));
    dbg!(p2(&input));
}
