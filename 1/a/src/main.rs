extern crate adjustmentlist;
use adjustmentlist::adjustmentlist::AdjustmentList;

fn main() {
    let accum = AdjustmentList::new().fold(0, |acc, x| acc + x);
    println!("{}", accum);
}
