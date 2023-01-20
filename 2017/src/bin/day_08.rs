use std::collections::HashMap;

#[derive(Debug)]
struct Condition {
    reg: String,
    cmp: String,
    amt: i32,
}

struct Instruction {
    reg: String,
    act: String,
    amt: i32,
    cond: Condition,
}

impl std::str::FromStr for Instruction {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let words: Vec<_> = s.split_whitespace().collect();

        Ok(Instruction {
            reg: words[0].to_string(),
            act: words[1].to_string(),
            amt: words[2].parse().unwrap(),
            cond: Condition {
                reg: words[4].to_string(),
                cmp: words[5].to_string(),
                amt: words[6].parse().unwrap(),
            },
        })
    }
}

fn p1(input: &str) -> i32 {
    let mut regs = HashMap::new();

    for Instruction {
        reg,
        act,
        amt,
        cond,
    } in input.lines().map(|s| s.parse().unwrap())
    {
        let x = regs.get(&cond.reg).unwrap_or(&0);
        if match cond.cmp.as_str() {
            "==" => x == &cond.amt,
            "!=" => x != &cond.amt,
            "<" => x < &cond.amt,
            "<=" => x <= &cond.amt,
            ">" => x > &cond.amt,
            ">=" => x >= &cond.amt,
            _ => unreachable!(),
        } {
            match act.as_str() {
                "inc" => *regs.entry(reg.clone()).or_insert(0) += amt,
                "dec" => *regs.entry(reg).or_insert(0) -= amt,
                _ => unreachable!(),
            };
        }
    }

    *regs.values().max().unwrap()
}

fn p2(input: &str) -> i32 {
    let mut regs = HashMap::new();
    let mut ret = i32::MIN;

    for Instruction {
        reg,
        act,
        amt,
        cond,
    } in input.lines().map(|s| s.parse().unwrap())
    {
        let x = regs.get(&cond.reg).unwrap_or(&0);
        if match cond.cmp.as_str() {
            "==" => x == &cond.amt,
            "!=" => x != &cond.amt,
            "<" => x < &cond.amt,
            "<=" => x <= &cond.amt,
            ">" => x > &cond.amt,
            ">=" => x >= &cond.amt,
            _ => unreachable!(),
        } {
            match act.as_str() {
                "inc" => *regs.entry(reg.clone()).or_insert(0) += amt,
                "dec" => *regs.entry(reg.clone()).or_insert(0) -= amt,
                _ => unreachable!(),
            };

            ret = ret.max(regs[&reg]);
        }
    }

    ret
}

fn main() {
    let input = std::fs::read_to_string("input/d8.txt").unwrap();
    dbg!(p1(&input));
    dbg!(p2(&input));
}
