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
            let nums: Vec<usize> = row.split_whitespace().map(|s| s.parse().unwrap()).collect();
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

pub fn main() {
    let input = std::fs::read_to_string("input/d2.txt").unwrap();
    dbg!(p1(&input));
    dbg!(p2(&input));
}
