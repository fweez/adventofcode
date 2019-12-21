import AOCShared
import Foundation
import Overture

/// To start a new day, copy and paste this file into a new dayX directory in Sources/
public typealias Map = [[Bool]]
public func parse(_ filename: String) -> Map {
    pipe(ingestFile, toMap)(filename)
}

func toMap(_ string: [String.SubSequence]) -> Map {
    string
        .map { $0.map { $0 == "#" }}
}

public func part1(_ field: Map) { print("Part 1: \(findBestMonitoringStationsSeenAsteroids(field))") }
public func part2(_ field: Map) { print("Part 2: ") }

typealias Field = Set<Point>

func allAsteroids(in map: Map) -> Field {
    Set(map
        .enumerated()
        .flatMap { y, row in
            Set(row
                .enumerated()
                .filter { _, b -> Bool in b } // has asteroid
                .map { x, _ -> Point in Point(x, y) })
        })
}

func findBestMonitoringStationsSeenAsteroids(_ map: Map) -> Int {
    let asteroidField = allAsteroids(in: map)
    let asteroids = asteroidField
        .map { ($0, visibleAsteroids(in: asteroidField, from: $0)) }
    
//    asteroids
//        .forEach { p, v in print("\(p): \(v)") }
//    
    return asteroids
        .map { $0.1 }
        .sorted()
        .last ?? Int.min
}

func visibleAsteroids(in field: Field, from center: Point) -> Int {
    field
        .map { visible(center, $0, in: field) }
        .filter { $0 }
        .count
}

var memoizedVisibility: [Set<Point>: Bool] = [:]

func visible(_ p1: Point, _ p2: Point, in field: Field) -> Bool {
    let ps = Set([p1, p2])
    if let b = memoizedVisibility[ps] { return b }
    
    let b = pointsBetween(p1, p2)
        .filter(field.contains) // if it's too slow, we know each of these points can see each other, and can memoize that information
        .isEmpty
    memoizedVisibility[ps] = b
    return b
}

var memoizedPointsBetween: [Set<Point>: [Point]] = [:]

func pointsBetween(_ p1: Point, _ p2: Point, _ precomputedSlope: Point? = nil) -> [Point] {
    if let a = memoizedPointsBetween[Set([p1, p2])] { return a }
    guard let m = precomputedSlope ?? slope(p1, p2) else { return [] }
    let n = p1 + m
    if n == p1 || n == p2 { return [] }
    let a = [n] + pointsBetween(n, p2, m)
    memoizedPointsBetween[Set([p1, p2])] = a
    return a
}

func slope(_ p1: Point, _ p2: Point) -> Point? {
    let dx = Double(p2.x - p1.x)
    let sgnDx = dx > 0 ? 1 : -1
    let dy = Double(p2.y - p1.y)
    let sgnDy = dy > 0 ? 1 : -1
    switch (dx, dy) {
    case (0, 0): return nil
    case (0, _): return Point(0, sgnDy)
    case (_, 0): return Point(sgnDx, 0)
    case (let dx, _) where abs(dx) >= 2:
        return (2...Int(abs(dx)))
            .map(pipe({ (i: Int) -> Double in Double(i) },
                      { (i: Double) -> (Double, Double) in (dx / i, dy / i) }))
            .filter { (fx: Double, fy: Double) -> Bool in
                return fx.rounded() == fx && fy.rounded() == fy
            }
            .map(pipe({ (Int($0), Int($1)) },
                      Point.init))
            .last
    default: return Point(Int(dx), Int(dy))
    }
}


