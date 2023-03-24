fn p1(grid: &[Vec<char>]) -> String {
    let mut pos: (i32, i32) = (0, grid[0].iter().position(|&c| c == '|').unwrap() as i32);
    let mut letters = String::new();

    let dirs: [(i32, i32); 4] = [(-1, 0), (0, 1), (1, 0), (0, -1)];
    let mut dir: i32 = 2;

    while let Some(&v) = grid
        .get(pos.0 as usize)
        .and_then(|row| row.get(pos.1 as usize))
    {
        match v {
            '+' => {
                let right = (dir + 1).rem_euclid(4);
                let new_pos = (
                    pos.0 + dirs[right as usize].0,
                    pos.1 + dirs[right as usize].1,
                );

                if let Some(&v) = grid
                    .get(new_pos.0 as usize)
                    .and_then(|row| row.get(new_pos.1 as usize))
                {
                    match v {
                        '|' | '-' | 'A'..='Z' => {
                            dir = right;
                        }
                        _ => {
                            dir = (dir - 1).rem_euclid(4);
                        }
                    }
                }
            }
            'A'..='Z' => {
                letters.push(v);
            }
            ' ' => {
                break;
            }
            _ => {}
        }

        pos = (pos.0 + dirs[dir as usize].0, pos.1 + dirs[dir as usize].1);
    }

    letters
}

fn p2(grid: &[Vec<char>]) -> usize {
    let mut pos: (i32, i32) = (0, grid[0].iter().position(|&c| c == '|').unwrap() as i32);
    let mut steps = 0;

    let dirs: [(i32, i32); 4] = [(-1, 0), (0, 1), (1, 0), (0, -1)];
    let mut dir: i32 = 2;

    while let Some(&v) = grid
        .get(pos.0 as usize)
        .and_then(|row| row.get(pos.1 as usize))
    {
        match v {
            '+' => {
                let right = (dir + 1).rem_euclid(4);
                let new_pos = (
                    pos.0 + dirs[right as usize].0,
                    pos.1 + dirs[right as usize].1,
                );

                if let Some(&v) = grid
                    .get(new_pos.0 as usize)
                    .and_then(|row| row.get(new_pos.1 as usize))
                {
                    match v {
                        '|' | '-' | 'A'..='Z' => {
                            dir = right;
                        }
                        _ => {
                            dir = (dir - 1).rem_euclid(4);
                        }
                    }
                }
            }
            ' ' => {
                break;
            }
            _ => {}
        }

        pos = (pos.0 + dirs[dir as usize].0, pos.1 + dirs[dir as usize].1);
        steps += 1;
    }

    steps
}

fn main() {
    let grid: Vec<_> = std::fs::read_to_string("input/d19.txt")
        .unwrap()
        .lines()
        .map(|s| s.chars().collect::<Vec<_>>())
        .collect();
    dbg!(p1(&grid));
    dbg!(p2(&grid));
}
