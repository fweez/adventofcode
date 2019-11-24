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

struct Worker {
    id: usize,
    task: Option<char>,
    remainingtime: u16,
}

const HEAD: char = '*';

fn insert(order_definition: InstructionOrder, before: &mut HashMap<char, HashSet<char>>, requires: &mut HashMap<char, HashSet<char>>) {
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

const WORKERCOUNT: usize = 5;

fn advance(workers: &mut Vec<Worker>, accum: &mut Vec<char>) -> u16 {
    let mut time = 0;
    workers.sort_by_key(|w| w.remainingtime);

    print!("workers: ");
    for idx in 0..workers.len() {
        let task = match workers[idx].task {
            Some(t) => t,
            None => '*'
        };
        print!("[{}: {} {}s]", workers[idx].id, task, workers[idx].remainingtime);
    }

    for idx in 0..workers.len() {
        if let Some(_) = workers[idx].task {
            time = workers[idx].remainingtime;
            break;
        }
    }
    for idx in 0..workers.len() {
        if workers[idx].remainingtime > 0 { 
            workers[idx].remainingtime -= time;
        }
        if workers[idx].remainingtime == 0 {
            if let Some(c) = workers[idx].task {
                accum.push(c);
            }
            workers[idx].task = None;
        }
    }
    println!("\nAdvanced {}", time);
    return time;
}

const A: u8 = 65;

fn main() {
    let mut before: HashMap<char, HashSet<char>> = HashMap::new();
    let mut requires: HashMap<char, HashSet<char>> = HashMap::new();

    before.insert(HEAD, HashSet::new());

    for order_definition in InputIterator::transform(|s| InstructionOrder::parse(s)) {
        insert(order_definition, &mut before, &mut requires);
    }

    let mut accum: Vec<char> = Vec::new();
    let mut curr: Vec<&char> = before.get(&HEAD).expect("NO HEAD?!").iter().collect();
    let mut workers: Vec<Worker> = Vec::new();
    for i in 0..WORKERCOUNT {
        workers.push(Worker { id: i, task: None, remainingtime: 0});
    }
    let mut totaltime = 0;
    loop {
        if curr.len() == 0 { break; }
        curr.sort();
        // Search through the whole set of candidates for a candidate we can complete
        let mut pushedcandidate = false;
        for i in 0..curr.len() {
            let candidate = curr[i];
            let mut requirements_satisfied = true;

            if let Some(candidate_requires) = requires.get(&candidate) {
                for c in candidate_requires {
                    if !accum.contains(c) { requirements_satisfied = false; }
                }
            }
            if requirements_satisfied {
                // Okay, theoretically we can complete this one. Do we have a worker available?
                let mut available_idx: usize = 99;
                for idx in 0..workers.len() {
                    match workers[idx].task {
                        Some(_) => continue,
                        None => {
                            available_idx = idx;
                            break;
                        }
                    }
                }
                if available_idx == 99 {
                    totaltime += advance(&mut workers, &mut accum);
                    for idx in 0..workers.len() {
                        if workers[idx].remainingtime == 0 {
                            available_idx = idx;
                            break;
                        }
                    }
                }
                workers[available_idx].task = Some(*candidate);
                workers[available_idx].remainingtime = (((*candidate as u8) - A) as u16) + 61;
                curr.remove(i);
                
                if let Some(children) = before.get(&candidate) {
                    for child in children {
                        if !curr.contains(&child) {
                            curr.push(child);
                        }
                    }
                }
                pushedcandidate = true;
                break;
            }
        }
        if !pushedcandidate {
            totaltime += advance(&mut workers, &mut accum);
        }
    }

    workers.sort_by_key(|w| w.remainingtime);
    loop {
        let t = advance(&mut workers, &mut accum);
        if t == 0 {
            break;
        }
        totaltime += t;
        
    }
    totaltime += workers.last().expect("...").remainingtime;


    print!("order (part a): ");
    for c in accum { print!("{}", c) }
    println!("");

    println!("total time (part b): {}", totaltime);
}
