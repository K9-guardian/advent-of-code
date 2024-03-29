// shamelessly stolen from reddit because i could not be fucked to actually solve this
// https://www.reddit.com/r/adventofcode/comments/7l78eb/2017_day_21_solutions/drk7mpw/
// thanks u/mythmon

use std::collections::HashMap;
use std::fmt;
use std::str::FromStr;

fn main() {
    let input = get_input();
    println!("{}", puzzle(input, 5));
    println!("{}", puzzle(input, 18));
}

fn get_input() -> &'static str {
    let input: &'static str = include_str!("../../input/d21.txt");
    input
}

fn puzzle(input: &str, iterations: usize) -> usize {
    let rules: PatternSet = input.parse().unwrap();
    let mut art = Grid::default();

    for _ in 0..iterations {
        let parts: Vec<Grid> = art.split().iter().map(|g| rules.apply_to(g)).collect();
        art = Grid::assemble_from(parts);
    }

    art.count()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Grid {
    cells: Vec<Vec<bool>>,
}

impl Grid {
    fn new(size: usize) -> Self {
        let mut row = Vec::with_capacity(size);
        row.resize(size, false);
        let mut cells = Vec::with_capacity(size);
        cells.resize(size, row);
        Self { cells }
    }

    fn assemble_from(parts: Vec<Grid>) -> Grid {
        let num_subgrids_wide = (parts.len() as f64).sqrt() as usize;
        assert_eq!(num_subgrids_wide * num_subgrids_wide, parts.len());
        let subgrid_size = parts[0].size();

        let mut rv = Grid::new(subgrid_size * num_subgrids_wide);

        for (idx, subgrid) in parts.iter().enumerate() {
            let gx = idx % num_subgrids_wide;
            let gy = idx / num_subgrids_wide;
            for x in 0..subgrid_size {
                for y in 0..subgrid_size {
                    rv.set(
                        (x + gx * subgrid_size, y + gy * subgrid_size),
                        subgrid.get((x, y)),
                    );
                }
            }
        }

        rv
    }

    fn size(&self) -> usize {
        self.cells.len()
    }

    fn get(&self, (x, y): (usize, usize)) -> bool {
        self.cells[y][x]
    }

    fn set(&mut self, (x, y): (usize, usize), val: bool) {
        self.cells[y][x] = val;
    }

    fn flip(&self) -> Self {
        let new_cells = self
            .cells
            .iter()
            .map(|row| row.iter().rev().map(|c| *c).collect())
            .collect();
        Self { cells: new_cells }
    }

    fn rotate(&self) -> Self {
        let l = self.size();
        let mut rv = Self::new(l);
        for x in 0..l {
            for y in 0..l {
                rv.set((y, l - x - 1), self.get((x, y)));
            }
        }
        rv
    }

    fn variants(&self) -> Vec<Grid> {
        let mut rv = Vec::with_capacity(8);
        rv.push(self.clone());
        rv.push(self.flip());
        for _ in 0..6 {
            let g = rv[rv.len() - 2].rotate();
            rv.push(g);
        }
        rv
    }

    fn split(&self) -> Vec<Grid> {
        let l = self.size();
        let m = if l % 2 == 0 {
            2
        } else if l % 3 == 0 {
            3
        } else {
            panic!(
                "Can't split grid of size {} (l % 2 == {}, l % 3 == {})",
                l,
                l % 2,
                l % 3
            );
        };
        let s = l / m;

        (0..(s * s))
            .map(|grid_idx| {
                let mut part = Grid::new(m);
                let gx = grid_idx % s;
                let gy = grid_idx / s;
                for x in 0..m {
                    for y in 0..m {
                        part.set((x, y), self.get((x + gx * m, y + gy * m)));
                    }
                }
                part
            })
            .collect()
    }

    fn count(&self) -> usize {
        self.cells
            .iter()
            .map(|row| row.iter().filter(|c| **c).count())
            .sum()
    }
}

impl Default for Grid {
    fn default() -> Self {
        Self {
            cells: vec![
                vec![false, true, false],
                vec![false, false, true],
                vec![true, true, true],
            ],
        }
    }
}

impl FromStr for Grid {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let cells: Vec<Vec<bool>> = input
            .split("/")
            .map(|row_string| {
                row_string
                    .chars()
                    .map(|c| match c {
                        '#' => true,
                        '.' => false,
                        _ => panic!("unexpected char in input: '{}'", c),
                    })
                    .collect()
            })
            .collect();
        Ok(Self { cells })
    }
}

impl fmt::Display for Grid {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        for row in self.cells.iter() {
            for cell in row.iter() {
                if *cell {
                    write!(formatter, "#")?;
                } else {
                    write!(formatter, ".")?;
                }
            }
            write!(formatter, "\n")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
struct PatternSet {
    patterns: HashMap<Grid, Grid>,
}

impl PatternSet {
    fn new() -> Self {
        Self {
            patterns: HashMap::new(),
        }
    }

    fn add_rule(&mut self, from: Grid, to: Grid) {
        for variant in from.variants() {
            self.patterns.insert(variant, to.clone());
        }
    }

    fn apply_to(&self, from: &Grid) -> Grid {
        self.patterns.get(from).unwrap_or(&from).clone()
    }
}

impl FromStr for PatternSet {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut rv = Self::new();

        for line in input.lines() {
            let mut parts: Vec<Grid> = line.split(" => ").map(|p| p.parse().unwrap()).collect();
            assert_eq!(parts.len(), 2);
            let to = parts.pop().unwrap();
            let from = parts.pop().unwrap();
            rv.add_rule(from, to);
        }

        Ok(rv)
    }
}

#[test]
fn test_example() {
    let input = "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#";
    assert_eq!(puzzle(input, 2), 12);
}

#[test]
fn pattern_set_passthrough() {
    let ps = PatternSet::new();
    let g1 = Grid::default();
    let g2 = ps.apply_to(&g1);
    assert_eq!(g1, g2);
}
