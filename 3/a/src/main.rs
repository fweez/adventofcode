extern crate inputiterator;
use inputiterator::inputiterator::InputIterator;

struct Rect {
    x: usize,
    y: usize,
    w: usize,
    h: usize,
    id: i32,
}
impl Rect {
    fn new(definition: String) -> Rect {
        // #1 @ 387,801: 11x22
        let mut elements = definition.split(|c| c == '#' || c == '@' || c == ',' || c == ':' || c == 'x');
        let _empty = elements.next().expect("no #!");
        let id = elements.next().expect("no id!").to_string();
        let x = elements.next().expect("no x!").to_string();
        let y = elements.next().expect("no y!").to_string();
        let w = elements.next().expect("no w!").to_string();
        let h = elements.next().expect("no h!").to_string();
        return Rect { 
            id: id.trim().parse::<i32>().expect("id didn't parse"),
            x: x.trim().parse::<usize>().expect("x didn't parse"),
            y: y.trim().parse::<usize>().expect("y didn't parse"),
            w: w.trim().parse::<usize>().expect("w didn't parse"),
            h: h.trim().parse::<usize>().expect("h didn't parse")
        };
    }
}

struct FabricCell {
    claims: i32
}

const FABRICSIZE: usize = 1500;

fn main() {
    let mut fabric: Vec<Vec<FabricCell>> = Vec::new();
    for i in 0..FABRICSIZE {
        fabric.push(Vec::new());
        for _j in 0..FABRICSIZE {
            fabric[i].push(FabricCell { claims: 0 });
        }
    }
    for rect in InputIterator::new().map(|s| Rect::new(s)) {
        for i in rect.x..(rect.x + rect.w) {
            for j in rect.y..(rect.y + rect.h) {
                fabric[i][j].claims += 1;
            }
        }
    }

    let claimedcount = fabric.iter()
        .fold(0, |a, v| a + v.iter()
            .fold(0, |b, cell| -> i32 { 
                if cell.claims >= 2 { return b + 1 }
                return b
             } ));
    
    println!("{}", claimedcount);
}
