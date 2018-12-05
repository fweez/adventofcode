use std::io::prelude::*;
use std::fs::File;

const CAPITALIZATION_OFFSET: i16 = 97-65;
const CR: u8 = 13;
const LF: u8 = 10;

fn destroys(a: u8, b: u8) -> bool { 
    ((a as i16) - (b as i16)).abs() == CAPITALIZATION_OFFSET 
}

fn main() {
    let file = File::open("input.txt").expect("Couldn't open file!");
    let mut output: Vec<u8> = Vec::new();
    for byte in file.bytes() {
        let next = match byte {
            Ok(b) => b,
            Err(_) => break
        };
        if next == CR || next == LF {
            break;
        }
        let destroy = match output.last() {
            Some(last) => destroys(next, *last),
            None => false
        };
        if destroy {
            output.pop();
        } else {
            output.push(next);
        }
    }

    println!("len: {}", output.len());

    // for byte in output {
    //     print!("{}", byte as char);
    // }
    // print!("\n");
}
