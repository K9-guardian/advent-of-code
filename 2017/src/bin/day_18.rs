use std::collections::{HashMap, VecDeque};

type State = HashMap<String, i64>;

fn p1(instructions: &[&str]) -> i64 {
    let mut regs: State = HashMap::new();
    let mut sound = 0;
    let mut idx: i64 = 0;

    fn lookup(regs: &HashMap<String, i64>, atom: &str) -> i64 {
        match atom.parse::<i64>() {
            Ok(val) => val,
            Err(_) => *regs.get(atom).unwrap_or(&0),
        }
    }

    while let Some(instr) = instructions.get(idx as usize) {
        let v: Vec<_> = instr.split_whitespace().collect();

        match v[0] {
            "snd" => {
                sound = lookup(&regs, v[1]);
            }
            "set" => {
                regs.insert(v[1].to_string(), lookup(&regs, v[2]));
            }
            "add" => {
                regs.insert(v[1].to_string(), lookup(&regs, v[1]) + lookup(&regs, v[2]));
            }
            "mul" => {
                regs.insert(v[1].to_string(), lookup(&regs, v[1]) * lookup(&regs, v[2]));
            }
            "mod" => {
                regs.insert(v[1].to_string(), lookup(&regs, v[1]) % lookup(&regs, v[2]));
            }
            "rcv" => {
                if lookup(&regs, v[1]) != 0 {
                    return sound;
                }
            }
            "jgz" => {
                if lookup(&regs, v[1]) > 0 {
                    idx += lookup(&regs, v[2]) - 1;
                }
            }
            _ => unreachable!(),
        }

        idx += 1;
    }

    unreachable!()
}

fn p2(instructions: &[&str]) -> usize {
    let (mut regs0, mut regs1) = (HashMap::new(), HashMap::new());
    let (mut q0, mut q1) = (VecDeque::new(), VecDeque::new());
    let (mut idx0, mut idx1): (i64, i64) = (0, 0);
    let mut snd_cnt = 0;

    fn lookup(regs: &HashMap<String, i64>, atom: &str) -> i64 {
        match atom.parse::<i64>() {
            Ok(val) => val,
            Err(_) => *regs.get(atom).unwrap_or(&0),
        }
    }

    fn run_prog(
        instructions: &[&str],
        idx: &mut i64,
        regs: &mut State,
        qr: &mut VecDeque<i64>,
        qs: &mut VecDeque<i64>,
        snd_cnt: &mut usize,
    ) {
        while let Some(instr) = instructions.get(*idx as usize) {
            let v: Vec<_> = instr.split_whitespace().collect();

            match v[0] {
                "snd" => {
                    qs.push_back(lookup(regs, v[1]));
                    *snd_cnt += 1;
                }
                "set" => {
                    regs.insert(v[1].to_string(), lookup(&regs, v[2]));
                }
                "add" => {
                    regs.insert(v[1].to_string(), lookup(&regs, v[1]) + lookup(&regs, v[2]));
                }
                "mul" => {
                    regs.insert(v[1].to_string(), lookup(&regs, v[1]) * lookup(&regs, v[2]));
                }
                "mod" => {
                    regs.insert(v[1].to_string(), lookup(&regs, v[1]) % lookup(&regs, v[2]));
                }
                "rcv" => match qr.pop_front() {
                    Some(val) => {
                        regs.insert(v[1].to_string(), val);
                    }
                    None => {
                        break;
                    }
                },
                "jgz" => {
                    if lookup(&regs, v[1]) > 0 {
                        *idx += lookup(&regs, v[2]) - 1;
                    }
                }
                _ => unreachable!(),
            }

            *idx += 1;
        }
    }

    regs0.insert("p".to_string(), 0);
    regs1.insert("p".to_string(), 1);

    while !(q0.is_empty() && instructions[idx0 as usize].starts_with("rcv")) {
        run_prog(
            &instructions,
            &mut idx0,
            &mut regs0,
            &mut q0,
            &mut q1,
            &mut 0,
        );
        run_prog(
            &instructions,
            &mut idx1,
            &mut regs1,
            &mut q1,
            &mut q0,
            &mut snd_cnt,
        );
    }

    snd_cnt
}

fn main() {
    let input = std::fs::read_to_string("input/d18.txt").unwrap();
    let instructions: Vec<_> = input.lines().collect();
    dbg!(p1(&instructions));
    dbg!(p2(&instructions));
}
