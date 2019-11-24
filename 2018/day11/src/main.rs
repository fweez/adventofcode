use std::i32;

fn cellvalue(x: usize, y: usize, serial: i32) -> i32 {
    if x >= 300 || y >= 300 { panic!("bad x/y"); }
    let rackid = (x + 10) as i32;
    let power = rackid * y as i32;
    let power = power + serial;
    let power = power * rackid;
    let power = power / 100;
    let power = power - ((power / 10) * 10);
    let power = power - 5;
    return power;
}

fn computeblockvalue(xbase: usize, ybase: usize, blocksize: usize, serial: i32) -> i32 {
    (xbase..xbase+blocksize).fold(0, |a, x| (ybase..ybase+blocksize).fold(a, |b, y| b + cellvalue(x, y, serial)))
}

fn findblockvalue(xbase: usize, ybase: usize, blocksize: usize, serial: i32, mut memo: &mut Vec<Vec<Vec<i32>>>) -> i32 {
    let v = memo[xbase][ybase][blocksize-1];
    if v != i32::MAX {
        return v;
    }
    if blocksize == 1 {
        let v = cellvalue(xbase, ybase, serial);
        memo[xbase][ybase][blocksize-1] = v;
        return v;
    }

    let newblock = blocksize / 2;
    let v1 = findblockvalue(xbase, ybase, newblock, serial, &mut memo);
    let v2 = findblockvalue(xbase + newblock, ybase, newblock, serial, &mut memo);
    let v3 = findblockvalue(xbase, ybase + newblock, newblock, serial, &mut memo);
    let v4 = findblockvalue(xbase + newblock, ybase + newblock, newblock, serial, &mut memo);
    let mut remaindersum = 0;
    let mut count = 0;
    if blocksize % 2 == 1 {
        let y = ybase + blocksize - 1;
        for x in xbase..(xbase + blocksize) {
            let v = findblockvalue(x, y, 1, serial, &mut memo);
            remaindersum += v;
            count += 1;
        }
        let x = xbase + blocksize - 1;
        for y in ybase..(ybase + blocksize - 1) {
            let v = findblockvalue(x, y, 1, serial, &mut memo);
            remaindersum += v;
            count += 1;
        }
    }
    if count + (4 * newblock * newblock) != blocksize * blocksize {
        panic!("Block sizes are wrong!");
    }
    let total = v1 + v2 + v3 + v4 + remaindersum;
    memo[xbase][ybase][blocksize-1] = total;
    return total;
}

fn newmemo() -> Vec<Vec<Vec<i32>>> {
    let mut memo: Vec<Vec<Vec<i32>>> = Vec::new();
    
    for y in 0..300 {
        let mut row = Vec::new();
        for x in 0..300 {
            let maxblocksize = 300 - y.max(x);
            let block = vec![i32::MAX; maxblocksize];
            row.push(block);
        }
        memo.push(row);
    }

    return memo;
}

fn maxblock(blocksize: usize, serial: i32) -> (usize, usize, i32) {
    let mut memo = newmemo();
    let mut maxblock: i32 = 0;
    let mut maxx: usize = 0;
    let mut maxy: usize = 0;
    for y in 0..300 {
        for x in 0..300 {
            let block = findblockvalue(x, y,  blocksize, serial, &mut memo);
            if block > maxblock {
                maxblock = block;
                maxx = x;
                maxy = y;
            }
        }
    }
    return (maxx, maxy, maxblock);
}

fn findmaxblock(serial: i32) -> (usize, usize, usize, i32) {
    let mut memo = newmemo();
    let mut maxblock: i32 = 0;
    let mut maxx: usize = 0;
    let mut maxy: usize = 0;
    let mut maxblocksize: usize = 0;

    for blocksize in 1..300 {
        for y in 0..300 {
            for x in 0..300 {
                if blocksize > 300 - y.max(x) { continue; }
                let block = findblockvalue(x, y, blocksize, serial, &mut memo);
                if block > maxblock {
                    maxblock = block;
                    maxx = x;
                    maxy = y;
                    maxblocksize = blocksize;
                }
            }
        }
    }
    return (maxx, maxy, maxblocksize, maxblock);
}

fn main() {
    let t = maxblock(3, 9798);
    println!("Part a: {},{} has value {}", t.0, t.1, t.2);
}

#[test]
fn testfinal() {
    let t = findmaxblock(9798);
    println!("Part b: {},{},{} has value {}", t.0, t.1, t.2, t.3);
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
    let t = maxblock(3, 18);
    println!("test1: {},{} has value {}", t.0, t.1, t.2);
    assert!(t == (33, 45, 29));
}

#[test]
fn test2() {
    let t = maxblock(3, 42);
    println!("test2: {},{} has value {}", t.0, t.1, t.2);
    assert!(t == (21, 61, 30));
}

#[test]
fn testcompute() {
    let serial = 18;
    let v = (90,269,16,113 as i32);
    let check = computeblockvalue(v.0, v.1, v.2, serial);
    assert_eq!(check, v.3);
}

#[test]
fn testfind1() {
    assert_eq!((90,269,16,113 as i32), findmaxblock(18));
}

#[test]
fn testfind2() {
    assert_eq!((232, 251, 12, 119 as i32), findmaxblock(42));
}

#[test]
fn testiteratively() {
    for blocksize in 6..300 {
        println!("blocksize {}", blocksize);
        let computed = computeblockvalue(0, 0, blocksize, 42);
        let memoized = findblockvalue(0, 0, blocksize, 42, &mut newmemo());
        assert_eq!(computed, memoized);
        println!("blocksize {} finished\n", blocksize);
    }
}

#[test]
fn testmemoizationreuse() {
    let mut memo = newmemo();
    for r in 4..200 {
        let blocksize = 204 - r;
        println!("blocksize {}", blocksize);
        let computed = computeblockvalue(0, 0, blocksize, 42);
        let memoized = findblockvalue(0, 0, blocksize, 42, &mut memo);
        assert_eq!(computed, memoized);
        println!("blocksize {} finished\n", blocksize);
    }
}

#[test]
fn testmemo() {
    let mut memo = newmemo();
    let serial = 42;
    memo[0][0][2] = 1;
    memo[0][2][2] = 1;
    memo[2][0][2] = -1;
    memo[2][2][2] = -1;
    assert_eq!(0, findblockvalue(0, 0, 4, serial, &mut memo));
    let remaindersum = cellvalue(4, 0, serial) + cellvalue(4, 1, serial) + cellvalue(4, 2, serial) + cellvalue(4, 3, serial) + cellvalue(4, 4, serial) + cellvalue(3, 4, serial) + cellvalue(2, 4, serial) + cellvalue(1, 4, serial) + cellvalue(0, 4, serial);
    assert_eq!(remaindersum, findblockvalue(0, 0, 5, serial, &mut memo));
}