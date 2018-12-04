extern crate inputiterator;
use inputiterator::inputiterator::InputIterator;

struct Rect {
    x: i32,
    y: i32,
    w: i32,
    h: i32,
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
            x: x.trim().parse::<i32>().expect("x didn't parse"),
            y: y.trim().parse::<i32>().expect("y didn't parse"),
            w: w.trim().parse::<i32>().expect("w didn't parse"),
            h: h.trim().parse::<i32>().expect("h didn't parse")
        };
    }

    fn zero() -> Rect { Rect { x: 0, y: 0, w: 0, h: 0, id: 0 } }

    fn intersection(&self, other: &Rect) -> Rect {
        let mut intersection: Rect = Rect::zero();
        if (self.x + self.w < other.x) || (other.x + other.w < self.x) { return Rect::zero() }
        if (self.y + self.h < other.y) || (other.y + other.h < self.y) { return Rect::zero() }
        intersection.x = self.x.max(other.x);
        let rightedge = (self.x + self.w).min(other.x + other.w);
        intersection.w = rightedge - intersection.x;
        if intersection.w < 0 { return Rect::zero() }

        intersection.y = self.y.max(other.y);
        let bottomedge = (self.y + self.h).min(other.y + other.h);
        intersection.h = bottomedge - intersection.y;
        if intersection.h < 0 { return Rect::zero() }
        return intersection;
    }

    fn complement(&self, other: &Rect) -> Vec<Rect> {
        let mut elements = Vec::new();
        
        // left side
        if self.x < other.x {
            let w = other.x.min(self.x + self.w);
            if w > 0 {
                elements.push(Rect { id: 0, x: self.x, y: self.y, w: w, h: self.h });
            }
        }
        // top
        if self.y < other.y {
            let h = other.y.min(self.y + self.h);
            if h > 0 {
                elements.push(Rect { id: 0, x: self.x, y: self.y, w: self.w, h: h});
            }
        }
        // right side
        if self.x + self.w > other.x + other.w {
            let x = other.x + other.w;
            let w = (self.x + self.w) - x;
            if w > 0 {
                elements.push(Rect { id: 0, x: x, y: self.y, w: w, h: self.h });
            }
        }
        //bottom
        if self.y + self.h > other.y + other.h {
            let y = other.y + other.h;
            let h = (self.y + self.h) - y;
            if h > 0 {
                elements.push(Rect { id: 0, x: self.x, y: y, w: self.w, h: h});
            }
        }

        return elements;
    }

    fn area(&self) -> i32 { self.h * self.w }
}

impl Clone for Rect {
    fn clone(&self) -> Rect { Rect {id: self.id, x: self.x, y: self.y, w: self.w, h: self.h } }
}

fn find_overlaps(rectangles: Vec<Rect>) -> Vec<Rect> {
    let mut overlaps: Vec<Rect> = Vec::new();
    for i in 0..(rectangles.len()-1) {
        let this_rect = &rectangles[i];
        for j in (i+1)..rectangles.len() {
//            println!("comparing elements {} and {}", i, j);
            let other_rect = &rectangles[j];
            let intersection = this_rect.intersection(other_rect);
            if intersection.area() == 0 { continue; }
            overlaps.push(intersection);
        }
    }
    return overlaps;
}

fn find_complements(rectangles: Vec<Rect>) -> Vec<Rect> {
    let mut complements: Vec<Rect> = Vec::new();
    let mut last_intersection: Rect = Rect::zero();
    println!("{} rectangles for complements", rectangles.len());
    if rectangles.len() == 1 {
        return rectangles;
    }
    for i in 0..(rectangles.len()-1) {
        println!("{}", i);
        let mut these_complements = Vec::new();
        let this_rect = rectangles[i].clone();
        these_complements.push(this_rect.clone());
        for j in (i+1)..rectangles.len() {
            println!("comparing elements {} and {}", i, j);
            let other_rect = &rectangles[j];
            let mut new_complements: Vec<Rect> = Vec::new();
            for mut c in &these_complements {
                let mut complements = c.complement(other_rect);
                new_complements.append(&mut complements);
            }
            println!("{}", new_complements.len());
            these_complements = new_complements;
            last_intersection = this_rect.intersection(other_rect);
        }
        complements.append(&mut these_complements);
    }
    complements.push(last_intersection);
    return complements;
}

fn main() {
    let input_rectangles: Vec<Rect> = InputIterator::new().map(|s| Rect::new(s)).collect();

    // calculate overlaps
    let overlaps = find_overlaps(input_rectangles);
    println!("{} overlaps", overlaps.len());
    let complements = find_complements(overlaps);
    let area = complements.iter().fold(0, |a, r| a + r.area());
    println!("area: {}", area);
}
