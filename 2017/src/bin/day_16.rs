#[derive(Clone, Debug)]
enum Move {
    Spin(usize),
    Exchange(usize, usize),
    Partner(u8, u8),
}

impl std::str::FromStr for Move {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Move::*;

        let (mov, rest) = s.split_at(1);

        match mov {
            "s" => Ok(Spin(rest.parse::<usize>().unwrap())),
            "x" => {
                let nums: Vec<_> = rest
                    .split('/')
                    .map(|s| s.parse::<usize>().unwrap())
                    .collect();
                Ok(Exchange(nums[0], nums[1]))
            }
            "p" => {
                let parts: Vec<_> = rest.split('/').map(|s| s.as_bytes()[0]).collect();
                Ok(Partner(parts[0], parts[1]))
            }
            _ => unreachable!(),
        }
    }
}

fn dance(progs: &mut [u8], moves: &[Move]) {
    use Move::*;

    for mov in moves {
        match mov {
            Spin(n) => progs.rotate_right(*n),
            Exchange(x, y) => progs.swap(*x, *y),
            Partner(p0, p1) => {
                let x = progs.iter().position(|&p| p == *p0).unwrap();
                let y = progs.iter().position(|&p| p == *p1).unwrap();
                progs.swap(x, y);
            }
        }
    }
}

fn p1(moves: &[Move]) -> String {
    let mut progs: Vec<_> = (0..16).map(|n| n as u8 + b'a').collect();

    dance(&mut progs, moves);

    String::from_utf8(progs).unwrap()
}

fn p2(moves: &[Move]) -> String {
    let mut progs: Vec<_> = (0..16).map(|n| n as u8 + b'a').collect();

    dance(&mut progs, moves);

    let mut order = 1;

    while !progs.iter().copied().eq((0..16).map(|n| n as u8 + b'a')) {
        dance(&mut progs, moves);
        order += 1;
    }

    for _ in 0..order {
        dance(&mut progs, moves);
    }

    let rem = 1000000000 % order;

    for _ in 0..rem {
        dance(&mut progs, moves);
    }

    String::from_utf8(progs).unwrap()
}

fn main() {
    let input: Vec<_> = std::fs::read_to_string("input/d16.txt")
        .unwrap()
        .split(',')
        .map(|s| s.parse::<Move>().unwrap())
        .collect();
    dbg!(p1(&input));
    dbg!(p2(&input));
}
