extern crate inputiterator;
use inputiterator::inputiterator::*;

struct Point {
    x: i16,
    y: i16,
    regionarea: i16
}

const FIELDSIZE: usize = 360;

impl Point {
    fn new(x: i16, y: i16) -> Point {
        Point { x, y, regionarea: 0 }
    }

    fn parse(definition: String) -> Point {
        let mut split = definition.split(", ");
        let x = split.next().expect("Didn't get x!");
        let x = x.trim().parse::<i16>().expect("Couldn't parse x!");
        let y = split.next().expect("Didn't get y!");
        let y = y.trim().parse::<i16>().expect("Couldn't parse y!");
        if x > FIELDSIZE as i16 || y > FIELDSIZE as i16 {
            panic!("Overflowed field!");
        }
        Self::new(x, y)
    }
}

fn manhattan(a: &Point, b: &Point) -> i16 { (a.x - b.x).abs() + (a.y - b.y).abs() }

const A: u8 = 65;
const LOWERA: u8 = 97;
const INFINITEIDX: usize = 53;
const EQUALIDX: usize = 54;

fn pointname(idx: usize) -> char {
    if idx == INFINITEIDX {
        return '*';
    }
    if idx == EQUALIDX {
        return '.';
    }
    if idx < 26 {
        return ((idx as u8) + A) as char;
    } else {
        return (((idx-26) as u8) + LOWERA) as char;
    }
}

const SHOWMAP: bool = false;

fn main() {
    let mut points: Vec<Point> = InputIterator::transform(|s| Point::parse(s)).collect();
    let max = FIELDSIZE as i16;
    for i in 0..FIELDSIZE {
        for j in 0..FIELDSIZE {
            let this = Point::new(i as i16, j as i16);
            let mut smallestdist = i16::max_value();
            let mut smallestidx = INFINITEIDX;
            for (idx, point) in points.iter().enumerate() {
                let dist = manhattan(point, &this);
                if dist == smallestdist {
                    smallestidx = EQUALIDX;
                } else if dist < smallestdist {
                    smallestdist = dist;
                    smallestidx = idx;
                }
            }
            
            let infpoints = [Point::new(i as i16, 0), Point::new(i as i16, max), Point::new(0, j as i16), Point::new(max, j as i16)];
            for infpoint in infpoints.iter() {
                let dist = manhattan(infpoint, &this);
                if dist < smallestdist {
                    smallestidx = INFINITEIDX;
                    break;
                }
            }
            if smallestidx != INFINITEIDX && smallestidx != EQUALIDX {
                points[smallestidx].regionarea += 1;
            }
            
            if SHOWMAP { print!("{}", pointname(smallestidx)); }
        }
        if SHOWMAP { print!("\n"); }
    }
    points.sort_by_key(|p| p.regionarea);
    println!("\nlargest area: {}", points.last().expect("should have some points!").regionarea);
}
