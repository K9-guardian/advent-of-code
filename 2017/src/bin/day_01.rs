use std::fs;

fn main() {
    let digits: Vec<_> = fs::read_to_string("input/d1.txt")
        .unwrap()
        .chars()
        .map(|c| char::to_digit(c, 10).unwrap())
        .collect();
    println!("{}", p1(&digits));
    println!("{}", p2(&digits));
}

fn p1(digits: &Vec<u32>) -> usize {
    let mut cnt: usize = 0;

    for i in 1..digits.len() {
        if digits.get(i - 1) == digits.get(i) {
            cnt += *digits.get(i).unwrap() as usize;
        }
    }

    if digits.first() == digits.last() {
        cnt += *digits.last().unwrap() as usize;
    }

    cnt
}

fn p2(digits: &Vec<u32>) -> usize {
    let mut cnt: usize = 0;

    for i in 0..digits.len() {
        let j = (i + digits.len() / 2) % digits.len();
        if digits.get(i) == digits.get(j) {
            cnt += *digits.get(i).unwrap() as usize;
        }
    }

    cnt
}
