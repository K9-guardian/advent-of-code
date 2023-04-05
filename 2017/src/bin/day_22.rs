use std::collections::HashMap;

#[derive(Clone, Copy, Debug)]
enum Node {
    Clean,
    Weakened,
    Infected,
    Flagged,
}

type State = HashMap<(i32, i32), Node>;

fn p1(st: &mut State) -> usize {
    let dirs: [(i32, i32); 4] = [(-1, 0), (0, 1), (1, 0), (0, -1)];
    let mut dir: usize = 0;
    let mut pos = (12, 12);
    let mut bursts = 0;

    for _ in 0..10000 {
        match st.get(&pos) {
            Some(Node::Infected) => {
                st.insert(pos, Node::Clean);
                dir = (dir + 1) % 4;
            }
            Some(Node::Clean) | None => {
                bursts += 1;
                st.insert(pos, Node::Infected);
                dir = (dir + 3) % 4;
            }
            _ => unreachable!(),
        }
        pos = (pos.0 + dirs[dir].0, pos.1 + dirs[dir].1);
    }

    bursts
}

fn p2(st: &mut State) -> usize {
    use Node::*;

    let dirs: [(i32, i32); 4] = [(-1, 0), (0, 1), (1, 0), (0, -1)];
    let mut dir: usize = 0;
    let mut pos = (12, 12);
    let mut bursts = 0;

    for _ in 0..10_000_000 {
        match st.get(&pos) {
            Some(Clean) | None => {
                st.insert(pos, Weakened);
                dir = (dir + 3) % 4;
            }
            Some(Weakened) => {
                bursts += 1;
                st.insert(pos, Infected);
            }
            Some(Infected) => {
                st.insert(pos, Flagged);
                dir = (dir + 1) % 4;
            }
            Some(Flagged) => {
                st.insert(pos, Clean);
                dir = (dir + 2) % 4;
            }
        }
        pos = (pos.0 + dirs[dir].0, pos.1 + dirs[dir].1);
    }

    bursts
}

fn main() {
    let mut input: State = HashMap::new();
    for (i, l) in std::fs::read_to_string("input/d22.txt")
        .unwrap()
        .lines()
        .enumerate()
    {
        for (j, c) in l.chars().enumerate() {
            input.insert(
                (i as i32, j as i32),
                match c {
                    '.' => Node::Clean,
                    '#' => Node::Infected,
                    _ => unreachable!(),
                },
            );
        }
    }
    dbg!(p1(&mut input.clone()));
    dbg!(p2(&mut input));
}
