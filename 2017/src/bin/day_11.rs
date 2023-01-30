use std::ops::AddAssign;

// https://www.redblobgames.com/grids/hexagons/#distances-cube
#[derive(Clone, Copy, Debug, Default)]
struct Coord(i32, i32, i32);

const DIRS: [Coord; 6] = [
    Coord(1, 0, -1),
    Coord(1, -1, 0),
    Coord(0, -1, 1),
    Coord(-1, 0, 1),
    Coord(-1, 1, 0),
    Coord(0, 1, -1),
];

impl AddAssign for Coord {
    fn add_assign(&mut self, rhs: Self) {
        *self = Self(self.0 + rhs.0, self.1 + rhs.1, self.2 + rhs.2);
    }
}

impl Coord {
    fn dist(&self) -> usize {
        ((self.0.abs() + self.1.abs() + self.2.abs()) / 2) as usize
    }
}

fn step(point: &mut Coord, dir: &str) {
    *point += DIRS[match dir {
        "n" => 0,
        "ne" => 1,
        "se" => 2,
        "s" => 3,
        "sw" => 4,
        "nw" => 5,
        _ => unreachable!(),
    }]
}

fn p1(input: &str) -> usize {
    let mut point = Coord::default();
    input.split(',').for_each(|dir| step(&mut point, dir));
    point.dist()
}

fn p2(input: &str) -> usize {
    let (mut point, mut ret) = (Coord::default(), 0);

    input.split(',').for_each(|dir| {
        step(&mut point, dir);
        ret = ret.max(point.dist())
    });

    ret
}

fn main() {
    let input = std::fs::read_to_string("input/d11.txt").unwrap();
    dbg!(p1(&input));
    dbg!(p2(&input));
}
