import AOCShared
import Foundation
import Overture

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

run(input: input, parse, part1, part2)

enum Instruction {
    case north(Int)
    case east(Int)
    case south(Int)
    case west(Int)
    case forward(Int)
    case left(Int)
    case right(Int)
}

typealias ParsedStructure = [Instruction]

func parse(_ input: String) -> ParsedStructure {
    let northParser = zip(literal("N"), intParser)
        .map { _, i in Instruction.north(i) }
    let southParser = zip(literal("S"), intParser)
        .map { _, i in Instruction.south(i) }
    let eastParser = zip(literal("E"), intParser)
        .map { _, i in Instruction.east(i) }
    let westParser = zip(literal("W"), intParser)
        .map { _, i in Instruction.west(i) }
    let forwardParser = zip(literal("F"), intParser)
        .map { _, i in Instruction.forward(i) }
    let leftParser = zip(literal("L"), intParser)
        .map { _, i in Instruction.left(i) }
    let rightParser = zip(literal("R"), intParser)
        .map { _, i in Instruction.right(i) }
    let dirParser = oneOf([northParser, southParser, eastParser, westParser, forwardParser, leftParser, rightParser])
    return input
        .split(separator: "\n")
        .compactMap { dirParser.runStatic(String($0)) }
}

enum Bearing: Int {
    case north
    case east
    case south
    case west
}

struct Ship {
    let bearing: Bearing
    let position: Point
    let waypoint: Point
}

func manhattan(_ ship: Ship) -> Int { abs(ship.position.x) +  abs(ship.position.y) }

func vector(_ bearing: Bearing, _ magnitude: Int) -> Point {
    switch bearing {
    case .north: return Point(0, 0 - magnitude)
    case .east: return Point(0 + magnitude, 0)
    case .south: return Point(0, 0 + magnitude)
    case .west: return Point(0 - magnitude, 0)
    }
}

func turn(_ bearing: Bearing, _ deg: Int) -> Bearing {
    var deg = deg
    if deg == 360 {
        return bearing
    } else if deg == 0 {
        return bearing
    } else if deg < 0 {
        deg = deg + 360
    }
    return Bearing(rawValue: (bearing.rawValue + (deg / 90)) % 4)!
}

func applyPart1(_ ship: Ship, _ instruction: Instruction) -> Ship {
    switch instruction {
    case .north(let i): return Ship(bearing: ship.bearing, position: ship.position + vector(.north, i), waypoint: Point(0, 0))
    case .east(let i): return Ship(bearing: ship.bearing, position: ship.position + vector(.east, i), waypoint: Point(0, 0))
    case .south(let i): return Ship(bearing: ship.bearing, position: ship.position + vector(.south, i), waypoint: Point(0, 0))
    case .west(let i): return Ship(bearing: ship.bearing, position: ship.position + vector(.west, i), waypoint: Point(0, 0))
    case .forward(let i): return Ship(bearing: ship.bearing, position: ship.position + vector(ship.bearing, i), waypoint: Point(0, 0))
    case .left(let i): return Ship(bearing: turn(ship.bearing, 0 - i), position: ship.position, waypoint: Point(0, 0))
    case .right(let i): return Ship(bearing: turn(ship.bearing, i), position: ship.position, waypoint: Point(0, 0))
    }
}

func part1(_ parsedInput: ParsedStructure) {
    let ship = parsedInput
        .reduce(Ship(bearing: .east, position: Point(0, 0), waypoint: Point(0, 0)), applyPart1)
    print("Part 1: \(manhattan(ship))")
}

func turnWaypoint(_ waypoint: Point, _ deg: Int) -> Point {
    switch deg {
    case 0, 360, -360: return waypoint
    case 90, -270: return Point(0 - waypoint.y, waypoint.x)
    case 180, -180: return Point(0, 0) - waypoint
    case 270, -90: return Point(waypoint.y, 0 - waypoint.x)
    default: fatalError()
    }
}

func applyPart2(_ ship: Ship, _ instruction: Instruction) -> Ship {
    switch instruction {
    case .north(let i): return Ship(bearing: ship.bearing, position: ship.position, waypoint: ship.waypoint + vector(.north, i))
    case .east(let i): return Ship(bearing: ship.bearing, position: ship.position, waypoint: ship.waypoint + vector(.east, i))
    case .south(let i): return Ship(bearing: ship.bearing, position: ship.position, waypoint: ship.waypoint + vector(.south, i))
    case .west(let i): return Ship(bearing: ship.bearing, position: ship.position, waypoint: ship.waypoint + vector(.west, i))
    case .forward(let i): return Ship(bearing: ship.bearing, position: ship.position + (ship.waypoint * i), waypoint: ship.waypoint)
    case .left(let i): return Ship(bearing: ship.bearing, position: ship.position , waypoint: turnWaypoint(ship.waypoint, 0 - i))
    case .right(let i): return Ship(bearing: ship.bearing, position: ship.position, waypoint: turnWaypoint(ship.waypoint, i))
    }
}

func part2(_ parsedInput: ParsedStructure) {
    let dist = manhattan(parsedInput
        .reduce(Ship(bearing: .east, position: Point(0, 0), waypoint: Point(10, -1)), applyPart2))
    print("Part 2: \(dist)")
}
