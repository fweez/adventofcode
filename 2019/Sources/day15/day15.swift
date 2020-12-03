import AOCShared
import Foundation
import Overture

public func parse(_ fileName: String) -> [Int] { parseFile(opcodeParser, fileName).first! }
public func part1(_ program: [Int]) { print("Part 1: \(movesToO2(program))") }
public func part2(_ program: [Int]) { print("Part 2: ") }

enum Direction: Int {
    case north = 1
    case south
    case west
    case east
    
    var offset: Point {
        switch self {
        case .north: return Point(0, -1)
        case .south: return Point(0, 1)
        case .west: return Point(-1, 0)
        case .east: return Point(1, 0)
        }
    }
}

extension Direction: CaseIterable { }

enum MapFeature: Int {
    case wall = 0
    case floor
    case o2 // remember, this means it moved
}

struct MapPoint {
    let kind: MapFeature
    let distance: Int
    let state: ProgramState
}

struct MapSearchStatus {
    let unvisited: [Point]
    let ship: [Point: MapPoint]
}

func movesToO2(_ program: [Int]) -> Int {
    produceMap(program)
        .first { $1.kind == .o2 }
        .map { $0.1.distance }
        ?? Int.min
}

// Map the level
func produceMap(_ program: [Int]) -> [Point: MapPoint] {
    // set unvisited to contain 0, 0 at distance 0
    let initialState = ProgramState(memory: program)
    let initialPoint = Point(0, 0)
    var searchStatus = MapSearchStatus(
        unvisited: [initialPoint],
        ship: [initialPoint: MapPoint(kind: .floor, distance: 0, state: initialState)])
    
    while true {
        print("Unvisited: \(searchStatus.unvisited)")
        guard let curr = searchStatus.unvisited.first, let currPointInfo = searchStatus.ship[curr] else { break }
        Direction.allCases.forEach { dir in
            let next = curr + dir.offset
            // If we already have a path to this next position with fewer steps than on this path, don't go that way
            guard (searchStatus.ship[next]?.distance ?? Int.max) > currPointInfo.distance + 1 else { return }
            // Feed the program this direction
            var state = currPointInfo.state
            state.inputs.append(dir.rawValue)
            // And run the program from our current position to try to move the robot to the next position
            guard let nextState = runIntcodeProgram(state) else { fatalError("Program Failed") }
            guard let out = nextState.outputs.last else { fatalError("No output!") }
            guard let nextFeature = MapFeature(rawValue: out) else { fatalError("Output \(out) was not a feature") }
            // Set the map & unvisited list
            var nextShip = searchStatus.ship
            var nextUnvisited = searchStatus.unvisited
            if nextFeature != .wall {
                nextShip[next] = MapPoint(kind: nextFeature, distance: currPointInfo.distance + 1, state: nextState)
                nextUnvisited.append(next)
            } else {
                nextShip[next] = MapPoint(kind: nextFeature, distance: currPointInfo.distance + 1, state: ProgramState(memory: []))
            }
            searchStatus = MapSearchStatus(
                unvisited: nextUnvisited,
                ship: nextShip)
        }
        
        searchStatus = MapSearchStatus(
            unvisited: Array(searchStatus.unvisited.suffix(from: 1)),
            ship: searchStatus.ship)
        
        printShip(curr, searchStatus.ship)
    }
    
    return searchStatus.ship
}

func printShip(_ robotPosition: Point, _ ship: [Point: MapPoint]) {
    let minX = ship.keys.reduce(Int.max) { min($0, $1.x) }
    let maxX = ship.keys.reduce(Int.min) { max($0, $1.x) }
    let minY = ship.keys.reduce(Int.max) { min($0, $1.y) }
    let maxY = ship.keys.reduce(Int.min) { max($0, $1.y) }
    print((minY..<maxY).map { y in
        (minX..<maxX).map { x in
            let p = Point(x, y)
            guard let i = ship[p] else { return "?" }
            if p == robotPosition { return "R" }
            switch i.kind {
            case .floor: return " "
            case .wall: return "#"
            case .o2: return "O"
            }
        }.joined()
    }.joined(separator: "\n"))
}
