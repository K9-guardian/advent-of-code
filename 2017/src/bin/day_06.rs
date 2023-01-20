use std::{collections::HashSet, rc::Rc};

fn p1(input: &[usize]) -> usize {
    let mut states = HashSet::new();
    let mut current_state = input.to_vec();

    while states.insert(current_state.clone()) {
        let (mut index, mut blocks) = current_state
            .iter()
            .copied()
            .enumerate()
            .rev()
            .max_by_key(|p| p.1)
            .unwrap();

        current_state[index] = 0;

        while blocks != 0 {
            index = (index + 1) % current_state.len();
            current_state[index] += 1;
            blocks -= 1;
        }
    }

    states.len()
}

fn p2(input: &[usize]) -> usize {
    let mut states = HashSet::new();
    let mut history = Vec::new();
    let mut current_state = input.to_vec();

    let mut rc = Rc::new(current_state.clone());

    while states.insert(Rc::clone(&rc)) {
        history.push(rc);

        let (mut index, mut blocks) = current_state
            .iter()
            .copied()
            .enumerate()
            .rev()
            .max_by_key(|p| p.1)
            .unwrap();

        current_state[index] = 0;

        while blocks != 0 {
            index = (index + 1) % current_state.len();
            current_state[index] += 1;
            blocks -= 1;
        }

        rc = Rc::new(current_state.clone());
    }

    history.into_iter().rev().take_while(|v| *v != rc).count() + 1
}

fn main() {
    let input: Vec<_> = std::fs::read_to_string("input/d6.txt")
        .unwrap()
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect();
    dbg!(p1(&input));
    dbg!(p2(&input));
}
