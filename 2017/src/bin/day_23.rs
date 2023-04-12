use std::collections::HashMap;

enum Atom {
    Reg(String),
    Val(i64),
}

impl std::str::FromStr for Atom {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.parse::<i64>() {
            Ok(val) => Atom::Val(val),
            Err(_) => Atom::Reg(s.to_string()),
        })
    }
}

enum Instr {
    Set(Atom, Atom),
    Sub(Atom, Atom),
    Mul(Atom, Atom),
    Jnz(Atom, Atom),
}

impl std::str::FromStr for Instr {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let split: Vec<_> = s.split_whitespace().collect();

        Ok(match split[0] {
            "set" => Instr::Set,
            "sub" => Instr::Sub,
            "mul" => Instr::Mul,
            "jnz" => Instr::Jnz,
            _ => unreachable!(),
        }(
            split[1].parse().unwrap(), split[2].parse().unwrap()
        ))
    }
}

fn p1(instrs: &[Instr]) -> usize {
    use Atom::*;
    use Instr::*;

    let mut idx: i64 = 0;
    let mut cnt = 0;
    let mut st = HashMap::new();

    fn lookup(st: &HashMap<String, i64>, a: &Atom) -> i64 {
        match a {
            Reg(r) => *st.get(r).unwrap_or(&0),
            Val(v) => *v,
        }
    }

    while let Some(instr) = instrs.get(idx as usize) {
        match instr {
            Set(Reg(x), y) => {
                st.insert(x.clone(), lookup(&st, y));
            }
            Sub(r @ Reg(x), y) => {
                let val = lookup(&st, r) - lookup(&st, y);
                st.insert(x.clone(), val);
            }
            Mul(r @ Reg(x), y) => {
                cnt += 1;
                let val = lookup(&st, r) * lookup(&st, y);
                st.insert(x.clone(), val);
            }
            Jnz(x, y) => {
                if lookup(&st, x) != 0 {
                    idx += lookup(&st, y) - 1;
                }
            }
            _ => unreachable!(),
        }

        idx += 1;
    }

    cnt
}

fn p2(_instrs: &[Instr]) -> usize {
    // From analyzing the code, we see that we want to find the numbers from 106500 and 123500 with
    // a step of 17 that are composite.

    let mut cnt = 0;

    'outer: for i in 0..=1000 {
        let n = 106_500 + i * 17;
        for d in 2..n {
            if n % d == 0 {
                cnt += 1;
                continue 'outer;
            }
        }
    }

    cnt
}

fn main() {
    let instrs: Vec<_> = std::fs::read_to_string("input/d23.txt")
        .unwrap()
        .lines()
        .map(|l| l.parse::<Instr>().unwrap())
        .collect();
    dbg!(p1(&instrs));
    dbg!(p2(&instrs));
}
