use std::collections::HashMap;

// TODO: Double pos and wrap around
#[derive(Debug, Clone, Copy)]
struct Layer {
    pos: i32,
    range: i32,
}

#[derive(Debug, Clone)]
struct Firewall(HashMap<i32, Layer>);

impl std::str::FromStr for Firewall {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut fw = HashMap::new();

        for l in s.lines() {
            let (depth, range) = l.split_once(": ").unwrap();

            fw.insert(
                depth.parse().unwrap(),
                Layer {
                    pos: 0,
                    range: range.parse().unwrap(),
                },
            );
        }

        Ok(Firewall(fw))
    }
}

#[derive(Debug, Clone)]
struct State {
    packet: i32,
    firewall: Firewall,
}

enum Status {
    Safe,
    Caught,
}

fn state_loop(
    State {
        packet,
        firewall: Firewall(fw),
    }: &mut State,
) -> Status {
    *packet += 1;

    let mut status = Status::Safe;

    if let Some(Layer { pos: 0, .. }) = fw.get(packet) {
        status = Status::Caught;
    }

    for Layer { pos, range } in fw.values_mut() {
        *pos = (*pos + 1) % ((*range - 1) * 2);
    }

    status
}

fn p1(st: &mut State) -> i32 {
    let end = *st.firewall.0.keys().max().unwrap();
    let mut ret = 0;

    for _ in 0..=end {
        if let Status::Caught = state_loop(st) {
            ret += st.firewall.0[&st.packet].range * st.packet;
        }
    }

    ret
}

fn p2(st: &mut State) -> i32 {
    let end = *st.firewall.0.keys().max().unwrap();
    let mut delay = 0;

    'outer: loop {
        st.packet = -1;
        for Layer { pos, range } in st.firewall.0.values_mut() {
            *pos = delay % ((*range - 1) * 2);
        }

        for _ in 0..=end {
            if let Status::Caught = state_loop(st) {
                delay += 1;
                continue 'outer;
            }
        }

        return delay;
    }
}

fn main() {
    let mut input = State {
        packet: -1,
        firewall: std::fs::read_to_string("input/d13.txt")
            .unwrap()
            .parse()
            .unwrap(),
    };
    dbg!(p1(&mut input.clone()));
    dbg!(p2(&mut input));
}
