use std::{collections::HashMap, fs};

fn p1(input: &str) -> usize {
    input
        .lines()
        .map(str::split_whitespace)
        .filter(|ws| {
            ws.clone()
                .enumerate()
                .all(|(i, x)| !ws.clone().skip(i + 1).any(|y| x == y))
        })
        .count()
}

fn p2(input: &str) -> usize {
    input
        .lines()
        .map(|s| {
            s.split_whitespace().map(|w| {
                let mut map: HashMap<char, usize> = HashMap::new();

                for c in w.chars() {
                    map.entry(c).and_modify(|n| *n += 1).or_insert(1);
                }

                map
            })
        })
        .filter(|ms| {
            ms.clone()
                .enumerate()
                .all(|(i, x)| !ms.clone().skip(i + 1).any(|y| x == y))
        })
        .count()
}

fn main() {
    let input = fs::read_to_string("input/d4.txt").unwrap();
    dbg!(p1(&input));
    dbg!(p2(&input));
}
