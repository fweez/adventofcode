import AOCShared
import Foundation
import Overture

/// To start a new day, copy and paste this file into a new dayX directory in Sources/
enum Direction {
    case up(Int)
    case right(Int)
    case down(Int)
    case left(Int)
}

let up: Parser<Direction, String> = zip(
    literal("U"),
    number
)
    .map(pipe({ $0.1 }, intify, Direction.up))

let right: Parser<Direction, String> = zip(
    literal("R"),
    number
)
    .map(pipe({ $0.1 }, intify, Direction.right))

let down: Parser<Direction, String> = zip(
    literal("D"),
    number
)
    .map(pipe({ $0.1 }, intify, Direction.down))

let left: Parser<Direction, String> = zip(
    literal("L"),
    number
)
    .map(pipe({ $0.1 }, intify, Direction.left))

let direction = oneOf([up, right, down, left])
let wire = zeroOrMore(direction, separatedBy: literal(","))

struct Point {
    var x: Int
    var y: Int
    var distance: Int
    
    init(_ x: Int, _ y: Int, _ distance: Int = 0) {
        self.x = x
        self.y = y
        self.distance = distance
    }
}

extension Point: Hashable {
    func hash(into hasher: inout Hasher) {
        hasher.combine(x)
        hasher.combine(y)
    }
}
extension Point: Equatable {
    static func ==(lhs: Point, rhs: Point) -> Bool {
        lhs.x == rhs.x && lhs.y == rhs.y
    }
}

let panelSize = 10000

func distToClosestIntersectionFromFile() -> Int {
    distToClosestIntersection(ingestFile("day3.txt"))
}

func distToLowestStepsFromFile() -> Int {
    distToLowestSteps(ingestFile("day3.txt"))
}

typealias Segment = (origin: Point, direction: Direction)

func textToWires(_ lines: [String.SubSequence]) -> [[Direction]] {
    lines
        .map {
            var line = $0
            let matches = wire.run(&line)!
            guard line.count == 0 else { preconditionFailure("Didn't completely parse. got \(matches), remaining '\(line)'") }
            return matches
        }
}

func generateSegments(_ directions: [Direction]) -> [Segment] {
    directions.reduce([], createSegments)
}

func createSegments(_ lastSegments: [Segment], _ direction: Direction) -> [Segment] {
    guard let lastSegment = lastSegments.last else { return [(origin: Point(0, 0), direction: direction)] }
    let new: Segment
    switch lastSegment.direction {
    case .up(let d):
        new = (origin: Point(lastSegment.origin.x, lastSegment.origin.y + d, lastSegment.origin.distance + d), direction: direction)
    case .right(let d):
        new = (origin: Point(lastSegment.origin.x + d, lastSegment.origin.y, lastSegment.origin.distance + d), direction: direction)
    case .down(let d):
        new = (origin: Point(lastSegment.origin.x, lastSegment.origin.y - d, lastSegment.origin.distance + d), direction: direction)
    case .left(let d):
        new = (origin: Point(lastSegment.origin.x - d, lastSegment.origin.y, lastSegment.origin.distance + d), direction: direction)
    }
    return lastSegments + [new]
}

func getAllIntersections(_ wireA: [Segment], _ wireB: [Segment]) -> [Point] {
    let wireAPoints: Set<Point> = Set(wireA
        .flatMap(pointsIn))
    let wireBPoints: Set<Point> = Set(wireB
        .flatMap(pointsIn))
    return Array(wireAPoints.intersection(wireBPoints)
        .compactMap { pA -> Point? in
            guard pA.x != 0 || pA.y != 0 else { return nil }
            guard let pB = wireBPoints.first(where: { $0 == pA }) else { return nil }
            return Point(pA.x, pA.y, pA.distance + pB.distance)
        })
}

func pointsIn(_ segment: Segment) -> [Point] {
    switch segment.direction {
    case .up(let d): return (segment.origin.y...(segment.origin.y + d))
        .enumerated()
        .map { Point(segment.origin.x, $1, segment.origin.distance + $0) }
    case .right(let d): return (segment.origin.x...(segment.origin.x + d))
        .enumerated()
        .map { Point($1, segment.origin.y, segment.origin.distance + $0) }
    case .down(let d): return ((segment.origin.y - d)...segment.origin.y)
        .enumerated()
        .map { Point(segment.origin.x, $1, segment.origin.distance + $0) }
    case .left(let d): return ((segment.origin.x - d)...segment.origin.x)
        .enumerated()
        .map { Point($1, segment.origin.y, segment.origin.distance + $0) }
    }
}

func manhattanDist(_ p1: Point, _ p2: Point = Point(0, 0)) -> Int {
    abs(p1.x - p2.x) + abs(p1.y - p2.y)
}

var intersections: [Point] = []
func distToClosestIntersection(_ lines: [String.SubSequence]) -> Int {
    let wires: [[Direction]] = textToWires(lines)
    let dirsA = wires.first!
    let dirsB = wires.last!
    let wireA = generateSegments(dirsA)
    let wireB = generateSegments(dirsB)
    intersections = getAllIntersections(wireA, wireB)
    return intersections
        .map { manhattanDist($0) }
        .reduce(Int.max, min)
}

func distToLowestSteps(_ lines: [String.SubSequence]? = nil) -> Int {
    if let lines = lines {
        let wires: [[Direction]] = textToWires(lines)
        let dirsA = wires.first!
        let dirsB = wires.last!
        let wireA = generateSegments(dirsA)
        let wireB = generateSegments(dirsB)
        intersections = getAllIntersections(wireA, wireB)
    }
    return intersections
        .map { $0.distance }
        .reduce(Int.max, min)
}

public func part1() { print("Part 1: \(distToClosestIntersectionFromFile())") }
public func part2() { print("Part 2: \(distToLowestSteps())") }

