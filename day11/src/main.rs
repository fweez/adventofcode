fn cellvalue(x: usize, y: usize, serial: i32) -> i32 {
    let rackid = (x + 10) as i32;
    let power = rackid * y as i32;
    let power = power + serial;
    let power = power * rackid;
    let power = power / 100;
    let power = power - ((power / 10) * 10);
    let power = power - 5;
    return power;
}

fn blockvalue(xbase: usize, ybase: usize, serial: i32) -> i32 {
    (xbase..xbase+3).fold(0, |a, x| (ybase..ybase+3).fold(a, |b, y| b + cellvalue(x, y, serial)))
}

fn maxblock(serial: i32) -> (usize, usize, i32) {
    let mut maxblock: i32 = 0;
    let mut maxx: usize = 0;
    let mut maxy: usize = 0;
    for y in 0..300 {
        for x in 0..300 {
            let block = blockvalue(x, y, serial);
            if block > maxblock {
                maxblock = block;
                maxx = x;
                maxy = y;
            }
        }
    }
    return (maxx, maxy, maxblock);
}

fn main() {
    let t = maxblock(9798);
    println!("Part a: {},{} has value {}", t.0, t.1, t.2);
}
 
#[test]
fn testcell1() {
    let v = cellvalue(33, 45, 18);
    assert_eq!(v, 4);
}

#[test]
fn testcell() {
    assert_eq!(cellvalue(3, 5, 8), 4);
}

#[test]
fn testblock1() {
    let t = maxblock(18);
    println!("test1: {},{} has value {}", t.0, t.1, t.2);
    assert!(t == (33, 45, 29));
}

#[test]
fn test2() {
    let t = maxblock(42);
    println!("test2: {},{} has value {}", t.0, t.1, t.2);
    assert!(t == (21, 61, 30));
}
