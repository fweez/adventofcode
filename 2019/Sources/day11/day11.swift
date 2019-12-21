import AOCShared
import Foundation
import Overture

public func parse(_ fileName: String) -> [Int] { parseFile(opcodeParser, fileName).first! }
public func part1(_ program: [Int]) { print("Part 1: \(countPaintedPanels(program))") }
public func part2(_ program: [Int]) { print("Part 2: \(drawHull(program))") }

enum Direction {
    case up
    case right
    case down
    case left
    
    var left: Direction {
        switch self {
        case .up: return .left
        case .right: return .up
        case .down: return .right
        case .left: return .down
        }
    }
    
    var right: Direction {
        switch self {
        case .up: return .right
        case .right: return .down
        case .down: return .left
        case .left: return .up
        }
    }
    
    var advance: Point {
        switch self {
        case .up: return Point(0, 1)
        case .right: return Point(1, 0)
        case .down: return Point(0, -1)
        case .left: return Point(-1, 0)
        }
    }
}

enum HullColor: Int {
    case black = 0
    case white
}

typealias Robot = (position: Point, direction: Direction)
typealias Hull = [Point: HullColor]

var hull: Hull = [:]

func buildComputer(_ program: [Int]) -> ProgramState {
    var robot: Robot = (Point(0, 0), .up)

    func paint(_ color: Int) {
        switch color {
        case 0: hull[robot.position] = .black
        case 1: hull[robot.position] = .white
        default: fatalError("Unexpected output for color \(color)")
        }
    }
    
    func turn(_ direction: Int) {
        switch direction {
        case 0: robot = (robot.position, robot.direction.left)
        case 1: robot = (robot.position, robot.direction.right)
        default: fatalError("Unexpected output for direction")
        }
        robot = (robot.direction.advance + robot.position, robot.direction)
    }
    
    var nextOutputIsPaint = true
    
    return ProgramState(
        memory: program,
        inputBlock: { color(under: robot, on: hull).rawValue },
        outputBlock: { out in
            if nextOutputIsPaint { paint(out) }
            else { turn(out) }
            nextOutputIsPaint = !nextOutputIsPaint
        })
}

func countPaintedPanels(_ program: [Int]) -> Int {
    hull = [:]
    _ = runIntcodeProgram(buildComputer(program))
    return hull.count
}

func color(under robot: Robot, on hull: Hull) -> HullColor { hull[robot.position] ?? .black }

func drawHull(_ program: [Int]) {
    hull = [Point(0, 0): .white]
    _ = runIntcodeProgram(buildComputer(program))
    
    let minX = hull
        .keys
        .map { $0.x }
        .reduce(Int.max, min)
    let maxX = hull
        .keys
        .map { $0.x }
        .reduce(Int.min, max)
    let minY = hull
        .keys
        .map { $0.y }
        .reduce(Int.max, min)
    let maxY = hull
        .keys
        .map { $0.y }
        .reduce(Int.min, max)
    print((minY...maxY)
        .reversed() // the robot thinks up is +(0, 1), not (0, -1)
        .map { y in
            (minX...maxX)
                .map { x in
                    switch hull[Point(x, y)] {
                    case .black: return " "
                    case .white: return "#"
                    case nil: return " "
                    }
                }
                .joined()
        }
        .joined(separator: "\n"))
}
