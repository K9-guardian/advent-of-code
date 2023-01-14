use std::fs;

fn p1(input: usize) -> usize {
    let (mut level, mut index) = (0, 1);

    while index * index <= input {
        level += 1;
        index += 2;
    }

    let shift = index * index - input;
    let width = level * 2 + 1;

    level + (level + 1) - (width - shift)
}

fn p2(input: usize) -> usize {
    let mut grid = vec![vec![0; 100]; 100];

    let (mut x, mut y) = (50, 50);
    let mut shift = 2;

    let directions = [(0, -1), (-1, 0), (0, 1), (1, 0)];

    grid[x][y] = 1;

    'outer: loop {
        x += 1;
        y += 1;

        for dir in directions {
            for _ in 0..shift {
                x = (x as i32 + dir.0) as usize;
                y = (y as i32 + dir.1) as usize;

                grid[x][y] = sum_neighbors(&grid, x, y);
                if grid[x][y] > input {
                    break 'outer;
                }
            }
        }

        shift += 2;
    }

    grid[x][y]
}

fn sum_neighbors(grid: &[Vec<usize>], x: usize, y: usize) -> usize {
    let mut cnt = 0;

    for i in (x - 1)..=(x + 1) {
        for j in (y - 1)..=(y + 1) {
            cnt += grid.get(i).and_then(|row| row.get(j)).unwrap_or(&0);
        }
    }

    cnt -= grid[x][y];
    cnt
}

fn main() {
    let input: usize = fs::read_to_string("input/d3.txt").unwrap().parse().unwrap();
    dbg!(p1(input));
    dbg!(p2(input));
}
