use std::io;
use std::collections::HashSet;

type Adjustment = i32;

struct AdjustmentList {
    
}

impl AdjustmentList {
    fn new() -> AdjustmentList {
        AdjustmentList { }
    }
}

impl Iterator for AdjustmentList {
    type Item = Adjustment;

    fn next(&mut self) -> Option<Self::Item> {
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Err(_error) => return None,
            Ok(_sz) => {
                let item = input.trim();
                if item.len() == 0 {
                    return None
                }
                let sign: i32 = match &item[0..1] {
                    "+" => 1,
                    "-" => -1,
                    _ => return None
                };
                let value = item[1..].parse::<i32>().unwrap() * sign;
                return Some(value);
            }
        }
    }
}

fn main() {
    let inputs: Vec<Adjustment> = AdjustmentList::new().collect();
    let mut seen_freqs = HashSet::new();
    let mut accum = 0;

    loop {
        for input in &inputs {
            accum += input;
            if seen_freqs.contains(&accum) {
                println!("{}", accum);
                return
            }
            seen_freqs.insert(accum);
        }
    }
}