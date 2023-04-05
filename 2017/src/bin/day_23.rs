use std::collections::HashMap;

enum Atom {
    Reg(String),
    Val(i32),
}

impl std::str::FromStr for Atom {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.parse::<i32>() {
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

    let mut idx: i32 = 0;
    let mut cnt = 0;
    let mut st: HashMap<String, i32> = HashMap::new();

    fn lookup(st: &HashMap<String, i32>, a: &Atom) -> i32 {
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

fn p2(instrs: &[Instr]) -> usize {
    // Strongly hinted we can solve this analytically
    // We see the offending line is "jnz a 2"
    // When we have a = 0, we skip the next 6 lines and start on 9
    // We see that we set b = 65 * 100 + 100000 and c = b + 17000
    // TODO: Analyze the code :p

    // We can see that the only mul instruction that gets executed is on
    // line 12 for part one.
    // The only instruction that modifies h is "sub h -1"
    // Thus, we want to find the number of times this instruction is run
    // Working backwards, we see that we want g to be 0
    0
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
