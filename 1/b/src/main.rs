extern crate adjustmentlist;
use adjustmentlist::adjustmentlist::Adjustment;
use adjustmentlist::adjustmentlist::AdjustmentList;
use std::collections::HashSet;

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