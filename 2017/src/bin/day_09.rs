use regex::Regex;

fn p1(input: &str) -> usize {
    let cancel = Regex::new(r"!.").unwrap();
    let garbage = Regex::new(r"<.*?>").unwrap();

    let clean0 = cancel.replace_all(input, "");
    let clean = garbage.replace_all(&clean0, "").replace(',', "");

    let mut score = 0;
    let mut level = 0;

    for c in clean.chars() {
        match c {
            '{' => {
                level += 1;
            }
            '}' => {
                score += level;
                level -= 1;
            }
            _ => unreachable!(),
        }
    }

    score
}

fn p2(input: &str) -> usize {
    let cancel = Regex::new(r"!.").unwrap();
    let garbage = Regex::new(r"<(.*?)>").unwrap();

    let clean0 = cancel.replace_all(input, "");

    garbage.captures_iter(&clean0).map(|s| s[1].len()).sum()
}

fn main() {
    let input = std::fs::read_to_string("input/d9.txt").unwrap();
    dbg!(p1(&input));
    dbg!(p2(&input));
}
