use std::io::prelude::*;
use std::fs::File;

const CAPITALIZATION_OFFSET: i16 = 97-65;
const CR: u8 = 13;
const LF: u8 = 10;
const A: u8 = 65;
const Z: u8 = 90;
const INVALID_CHAR: u8 = 91;

fn destroys(a: u8, b: u8) -> bool { 
    ((a as i16) - (b as i16)).abs() == CAPITALIZATION_OFFSET 
}

fn react(polymer: &Vec<u8>, scrub: Option<u8>) -> usize {
    let mut output: Vec<u8> = Vec::new();
    let scrub = scrub.unwrap_or(INVALID_CHAR);
    for &next in polymer {
        if next == CR || next == LF { break }
        if next ==  scrub || destroys(next, scrub) { continue }
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
    return output.len();
}

fn main() {
    let file = File::open("input.txt").expect("Couldn't open file!");
    let input = file.bytes().map(|b| b.unwrap()).collect();
    let part_a = react(&input, None);
    println!("fully reacted polymer len: {}", part_a);

    let mut min_char = INVALID_CHAR;
    let mut min_size = usize::max_value();
    for c in A..Z {
        let reacted_size = react(&input, Some(c));
        if reacted_size < min_size {
            min_char = c;
            min_size = reacted_size;
        }
    }
    println!("after removing unit {} and reacting, len: {}", min_char as char, min_size);
}
