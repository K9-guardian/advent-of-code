use std::fs;

pub mod day_01 {
    pub fn p1(digits: &[u32]) -> u32 {
        let mut cnt = 0;

        for p in digits.windows(2) {
            if p[0] == p[1] {
                cnt += p[0];
            }
        }

        if digits.first() == digits.last() {
            cnt += digits[0];
        }

        cnt
    }

    pub fn p2(digits: &[u32]) -> u32 {
        let mut cnt = 0;

        for i in 0..digits.len() {
            let j = (i + digits.len() / 2) % digits.len();
            if digits[i] == digits[j] {
                cnt += digits[i];
            }
        }

        cnt
    }
}

fn main() {
    let digits: Vec<_> = fs::read_to_string("input/d1.txt")
        .unwrap()
        .chars()
        .map(|c| char::to_digit(c, 10).unwrap())
        .collect();
    println!("{}", day_01::p1(&digits));
    println!("{}", day_01::p2(&digits));
}
