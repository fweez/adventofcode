use std::io;


fn main() {
    let mut accum = 0;

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
        accum += value;
    }
    println!("{}", accum);
}
