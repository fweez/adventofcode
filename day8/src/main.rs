use std::io::prelude::*;
use std::fs::File;
use std::str::Split;

struct Node {
    idx: usize,
    children: Vec<Box<Node>>,
    metadata: Vec<usize>
}

impl Node {
    fn new(idx: usize, mut stream: &mut Split<'_, char>) -> Node {
        let expected_children = stream.next().expect("No next!").parse::<usize>().expect("Couldn't parse expected children!");
        let metadata_count = stream.next().expect("No next!").parse::<usize>().expect("Couldn't parse metadata count!");
        //println!("New node {} will have {} children and {} metadata entries", idx, expected_children, metadata_count);
        let mut children = Vec::new();
        if expected_children > 0 {
            for child_idx in 0..expected_children {
                children.push(Box::new(Node::new(idx + child_idx + 1, &mut stream)));
            }
        }
        let mut metadata = Vec::new();
        if metadata_count > 0 {
            for _ in 0..metadata_count {
                let input = stream.next().expect("No next!").trim();
                let metadatum = match input.parse::<usize>() {
                    Err(err) => {
                        println!("Couldn't parse next integer '{}': {:?}", input, err);
                        panic!("bang");
                    },
                    Ok(v) => v
                };
                metadata.push(metadatum);
            }
         }
        //println!("New node: {}, {} children, metadata: {:?}", idx, children.len(), metadata);
        Node { idx, children, metadata }
    }

    fn sum_metadata(&self) -> u16 {
        self.metadata.iter().fold(0, |a, m| a + (*m as u16)) + self.children.iter().fold(0, |a, n| a + n.sum_metadata())
    }

    fn sum_metadata_with_indices(&self) -> u16 {
        if self.children.len() == 0 { return self.sum_metadata(); }
        let mut sum = 0;
        for idx in 0..self.metadata.len() {
            let idx = self.metadata[idx];
            if idx == 0 { continue; }
            let idx = idx - 1;
            if idx < self.children.len() { 
                sum += self.children[idx].sum_metadata_with_indices();
            }
        }
        return sum;
    }
}

fn main() {
    // I'd love to stream this instead of storing it, but fuckit:
    let mut file = File::open("input.txt").expect("Couldn't open input file");
    let mut input = String::new();
    file.read_to_string(&mut input).expect("Couldn't read input file");

    let mut split = input.split(' ');
    let head = Node::new(0, &mut split);
    println!("Metadata sum (part A): {}", head.sum_metadata());
    println!("Indexed metadata sum (part B): {}", head.sum_metadata_with_indices());
}
