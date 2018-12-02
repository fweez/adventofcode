use std::io;
extern crate boxids;
use boxids::BoxIDList;

fn main() {
    let mut box_ids = BoxIDList::new();
    loop {
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Err(_error) => break,
            Ok(_sz) => {
                let item = input.trim();
                if item.len() == 0 {
                    break
                }
                box_ids.insert(item);
            }
        }
    }
    println!("Checksum: {}", box_ids.checksum());
}
