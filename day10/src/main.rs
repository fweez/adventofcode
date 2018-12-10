extern crate inputiterator;
use inputiterator::inputiterator::*;
use std::{i32, u64};

struct Point {
    x: i32,
    y: i32,
    vx: i32,
    vy: i32
}

impl Point {
    fn splitxy(input: String) -> (i32, i32) {
        let mut split = input.split(',');
        let x = split.next().unwrap();
        let x = x.to_string();
        let x = x.trim();
        let x = x.parse::<i32>().unwrap();
        let y = split.next().unwrap();
        let y = y.to_string();
        let y = y.trim();
        let y = y.parse::<i32>().unwrap();
        return (x, y);
    }
    fn parse(definition: String) -> Point {
        //position=< 10775, -31651> velocity=<-1,  3>
        let mut split = definition.split(|s| s == '<' || s == '>');
        split.next();
        let (x, y) = Self::splitxy(split.next().unwrap().to_string());
        split.next();
        let (vx, vy) = Self::splitxy(split.next().unwrap().to_string());
        return Point { x, y, vx, vy };
    }
}

fn main() {
    let mut points: Vec<Point> = InputIterator::transform(|s| Point::parse(s)).collect();
    let mut t = 0;
    let mut t_minarea = 0;
    let mut minimumarea: u64 = u64::MAX;
    let mut t_min_minx: i32 = 0;
    let mut t_min_maxx: i32 = 0;
    let mut t_min_miny: i32 = 0;
    let mut t_min_maxy: i32 = 0;

    loop {
        t += 1;
        let mut minx = i32::MAX;
        let mut miny = i32::MAX;
        let mut maxx: i32 = 0;
        let mut maxy: i32 = 0;
        for i in 0..points.len() {
            points[i].x += points[i].vx;
            points[i].y += points[i].vy;
            if points[i].x > maxx { maxx = points[i].x.clone() }
            if points[i].x < minx { minx = points[i].x.clone() }
            if points[i].y > maxy { maxy = points[i].y.clone() }
            if points[i].y < miny { miny = points[i].y.clone() }
        }
        
        let width: u64 = (maxx - minx).abs() as u64;
        let height: u64 = (maxy - miny).abs() as u64;
        let thisarea = width * height;
        if thisarea < minimumarea {
            minimumarea = thisarea;
            t_minarea = t;
            t_min_minx = minx;
            t_min_maxx = maxx;
            t_min_miny = miny;
            t_min_maxy = maxy;
        } else {
            // we're going back up! reverse the last frame
            for i in 0..points.len() {
                points[i].x -= points[i].vx;
                points[i].y -= points[i].vy;
            }
            break
        }
    }

    println!("Min area found at time {}. x {} to {} ({} w); y {} to {} ({} h)", 
        t_minarea, t_min_minx, t_min_maxx, t_min_maxx - t_min_minx, t_min_miny, t_min_maxy, t_min_maxy - t_min_miny);
    let mut art: Vec<Vec<char>> = Vec::new();
    for _y in 0..(t_min_maxy - t_min_miny + 1) {
        let mut s = Vec::new();
        for _x in 0..(t_min_maxx - t_min_minx + 1) {
            s.push('.');
        }
        art.push(s);
    }
    for point in points {
        art[(point.y - t_min_miny) as usize][(point.x - t_min_minx) as usize] = '#';
    }
    for line in art {
        for c in line { print!("{}", c); }
        println!();
    }
}
