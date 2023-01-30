const SIZE: usize = 256;

fn knot_hash(circle: &mut [usize], n: usize, pos: &mut usize, skip: &mut usize) {
    let indices = (*pos..(*pos + n)).map(|i| i % SIZE);
    let reversed: Vec<_> = indices.clone().map(|i| circle[i]).rev().collect();
    indices.zip(reversed).for_each(|(i, n)| circle[i] = n);

    *pos = (*pos + n + *skip) % SIZE;
    *skip += 1;
}

fn p1(input: &str) -> usize {
    let mut circle: Vec<_> = (0..SIZE).collect();
    let (mut pos, mut skip) = (0, 0);

    input
        .split(',')
        .map(|s| s.parse::<usize>().unwrap())
        .for_each(|n| knot_hash(&mut circle, n, &mut pos, &mut skip));

    circle[0] * circle[1]
}

fn p2(input: &str) -> String {
    let lengths: Vec<usize> = input
        .bytes()
        .chain(vec![17, 31, 73, 47, 23])
        .map(|n| n as usize)
        .collect();

    let mut circle: Vec<_> = (0..SIZE).collect();
    let (mut pos, mut skip) = (0, 0);

    for _ in 0..64 {
        for &n in &lengths {
            knot_hash(&mut circle, n, &mut pos, &mut skip);
        }
    }

    circle
        .chunks(16)
        .map(|c| format!("{:02x}", c.iter().fold(0, |x, y| x ^ y)))
        .collect()
}

fn main() {
    let input = std::fs::read_to_string("input/d10.txt").unwrap();
    dbg!(p1(&input));
    dbg!(p2(&input));
}
