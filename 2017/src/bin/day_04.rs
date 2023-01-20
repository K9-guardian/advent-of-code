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
                let mut chars = [0; 26];

                for c in w.chars() {
                    chars[c as usize - 'a' as usize] += 1;
                }

                chars
            })
        })
        .filter(|ws| {
            ws.clone()
                .enumerate()
                .all(|(i, x)| !ws.clone().skip(i + 1).any(|y| x == y))
        })
        .count()
}

fn main() {
    let input = std::fs::read_to_string("input/d4.txt").unwrap();
    dbg!(p1(&input));
    dbg!(p2(&input));
}
