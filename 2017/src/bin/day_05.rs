use std::fs;

fn p1(input: &mut [i32]) -> usize {
    let mut ptr: i32 = 0;
    let mut steps = 0;

    while let Some(n) = input.get_mut(ptr as usize) {
        ptr += *n;
        *n += 1;
        steps += 1;
    }

    steps
}

fn p2(input: &mut [i32]) -> usize {
    let mut ptr: i32 = 0;
    let mut steps = 0;

    while let Some(n) = input.get_mut(ptr as usize) {
        ptr += *n;
        if *n >= 3 {
            *n -= 1
        } else {
            *n += 1;
        }
        steps += 1;
    }

    steps
}

fn main() {
    let mut input: Vec<_> = fs::read_to_string("input/d5.txt")
        .unwrap()
        .lines()
        .map(|s| s.parse().unwrap())
        .collect();
    dbg!(p1(&mut input.clone()));
    dbg!(p2(&mut input));
}
