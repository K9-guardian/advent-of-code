use itertools::Itertools;

const GRID_SIZE: usize = 128;
const KNOT_SIZE: usize = 256;

fn knot_hash(circle: &mut [usize], n: usize, pos: &mut usize, skip: &mut usize) {
    let indices = (*pos..(*pos + n)).map(|i| i % KNOT_SIZE);
    let reversed: Vec<_> = indices.clone().map(|i| circle[i]).rev().collect();
    indices.zip(reversed).for_each(|(i, n)| circle[i] = n);

    *pos = (*pos + n + *skip) % KNOT_SIZE;
    *skip += 1;
}

fn make_grid(input: &str) -> [[bool; GRID_SIZE]; GRID_SIZE] {
    let mut grid = [[false; GRID_SIZE]; GRID_SIZE];

    let mut circle = vec![0; 256];

    for i in 0..GRID_SIZE {
        (0..KNOT_SIZE).enumerate().for_each(|(i, n)| circle[i] = n);
        let (mut pos, mut skip) = (0, 0);

        let lengths: Vec<_> = format!("{}-{}", input, i)
            .bytes()
            .chain(vec![17, 31, 73, 47, 23])
            .collect();

        for _ in 0..64 {
            for &n in &lengths {
                knot_hash(&mut circle, n as usize, &mut pos, &mut skip);
            }
        }

        circle
            .chunks(16)
            .flat_map(|c| {
                format!("{:08b}", c.iter().fold(0, |x, y| x ^ y))
                    .bytes()
                    .collect::<Vec<_>>()
            })
            .enumerate()
            .for_each(|(j, n)| grid[i][j] = n != b'0');
    }

    grid
}

fn p1(input: &str) -> usize {
    make_grid(input).iter().flatten().filter(|&&b| b).count()
}

fn p2(input: &str) -> usize {
    let grid = make_grid(input);

    let mut stack = Vec::new();
    let mut seen = [[false; GRID_SIZE]; GRID_SIZE];
    let mut ret = 0;
    let shifts = [(-1, 0), (0, 1), (1, 0), (0, -1)];

    for (i, j) in (0..GRID_SIZE)
        .cartesian_product(0..GRID_SIZE)
        .filter(|&(i, j)| grid[i][j])
    {
        if !seen[i][j] {
            ret += 1;
        }

        stack.push((i, j));
        while let Some((i, j)) = stack.pop() {
            if !seen[i][j] {
                seen[i][j] = true;

                for (x, y) in shifts
                    .iter()
                    .map(|(x, y)| ((i as i32 + x) as usize, (j as i32 + y) as usize))
                {
                    if *grid.get(x).and_then(|row| row.get(y)).unwrap_or(&false) {
                        stack.push((x, y));
                    }
                }
            }
        }
    }

    ret
}

fn main() {
    let input = std::fs::read_to_string("input/d14.txt").unwrap();
    dbg!(p1(&input));
    dbg!(p2(&input));
}
