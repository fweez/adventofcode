use std::io;
use std::collections::HashSet;

fn main() {
    let mut inputs = Vec::new();
    
    loop {
        let mut input = String::new();
        io::stdin().read_line(&mut input)
            .expect("Didn't read line");
        let item = input.trim();
        if item.len() == 0 {
            break
        }
        let sign: i32 = match &item[0..1] {
            "+" => 1,
            "-" => -1,
            _ => break
        };
        let value = item[1..].parse::<i32>().unwrap() * sign;
        inputs.push(value);
    }
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
