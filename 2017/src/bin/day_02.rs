use std::fs;

pub mod day_02 {
    pub fn p1(input: &str) -> usize {
        input
            .lines()
            .map(|row| {
                let nums = row.split_whitespace().map(|s| s.parse::<usize>().unwrap());
                nums.clone().max().unwrap() - nums.min().unwrap()
            })
            .sum()
    }

    pub fn p2(input: &str) -> usize {
        input
            .lines()
            .map(|row| {
                let nums: Vec<_> = row
                    .split_whitespace()
                    .map(|s| s.parse::<usize>().unwrap())
                    .collect();
                for i in 0..nums.len() {
                    for j in (i + 1)..nums.len() {
                        let min = nums[i].min(nums[j]);
                        let max = nums[i].max(nums[j]);

                        if max % min == 0 {
                            return max / min;
                        }
                    }
                }
                unreachable!()
            })
            .sum()
    }
}

fn main() {
    let input = fs::read_to_string("input/d2.txt").unwrap();
    println!("{}", day_02::p1(&input));
    println!("{}", day_02::p2(&input));
}
