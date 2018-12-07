extern crate inputiterator;
use inputiterator::inputiterator::InputIterator;
use std::collections::{HashMap, HashSet};

struct InstructionOrder {
    this: char,
    child: char,
}

impl InstructionOrder {
    fn parse(definition: String) -> InstructionOrder {
        // Step L must be finished before step F can begin.
        let split: Vec<&str> = definition.split(' ').collect();
        let this = split[1].to_string().chars().last().expect("Didn't get a 'this' character");
        let child = split[7].to_string().chars().last().expect("Didn't get a child character");
        InstructionOrder { this, child }
    }
}

fn main() {
    const HEAD: char = '*';
    let mut before: HashMap<char, HashSet<char>> = HashMap::new();
    before.insert(HEAD, HashSet::new());
    let mut requires: HashMap<char, HashSet<char>> = HashMap::new();

    for order_definition in InputIterator::transform(|s| InstructionOrder::parse(s)) {
        if before.get(&HEAD).expect("NO HEAD!?").contains(&order_definition.child) {
            let mut head: HashSet<char> = before.get(&HEAD).expect("NO HEAD!?").iter().cloned().collect();
            head.remove(&order_definition.child);
            before.insert(HEAD, head);
        }
        let mut inhead = true;
        for v in before.values() {
            if v.contains(&order_definition.this) {
                inhead = false;
                break;
            }
        }
        if inhead {
            let mut head: HashSet<char> = before.get(&HEAD).expect("NO HEAD!?").iter().cloned().collect();
            head.insert(order_definition.this);
            before.insert(HEAD, head);
        }
        before.entry(order_definition.this).or_insert(HashSet::new()).insert(order_definition.child);
        requires.entry(order_definition.child).or_insert(HashSet::new()).insert(order_definition.this);
    }

    let mut accum: Vec<char> = Vec::new();
    let mut curr: Vec<&char> = before.get(&HEAD).expect("NO HEAD?!").iter().collect();
    loop {
        if curr.len() == 0 { break; }
        curr.sort();
        for i in 0..curr.len() {
            let candidate = curr[i];
            let mut requirements_satisfied = true;

            if let Some(candidate_requires) = requires.get(&candidate) {
                for c in candidate_requires {
                    if !accum.contains(c) { requirements_satisfied = false; }
                }
            }
            if requirements_satisfied {
                curr.remove(i);
                accum.push(*candidate);
                if let Some(children) = before.get(&candidate) {
                    for child in children {
                        if !curr.contains(&child) {
                            curr.push(child);
                        }
                    }
                }
                break;
            }
        }
    }

    for c in accum { print!("{}", c) }
    println!("");
}
