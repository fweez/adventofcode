import AOCShared
import Foundation
import Overture

public func parse(_ fileName: String) -> [Int] { parseFile(opcodeParser, fileName).first! }
public func part1(_ program: [Int]) { print("Part 1: \(countBlockTiles(program))") }
public func part2(_ program: [Int]) { print("Part 2: \(runVideoGame(program))") }

enum TileId: Int {
    case empty = 0
    case wall
    case block
    case hPaddle
    case ball
    
    case error = 99999
}

struct Tile {
    var position: Point
    var id: TileId = .empty
}

extension Tile: Hashable {
    func hash(into hasher: inout Hasher) {
        hasher.combine(position)
    }
}

enum TileData {
    case x
    case y
    case id
    
    var next: TileData {
        switch self {
        case .x: return .y
        case .y: return .id
        case .id: return .x
        }
    }
}

func countBlockTiles(_ program: [Int]) -> Int {
    Set(
        runIntcodeProgram(ProgramState(memory: program))
            .map { endState -> [Tile] in
                stride(from: 0, to: endState.outputs.count, by: 3)
                    .map { idx -> Tile in
                        Tile(
                            position: Point(endState.outputs[idx], endState.outputs[idx + 1]),
                            id: TileId.init(rawValue: endState.outputs[idx + 2]) ?? .error)
                    }
            } ?? [Tile(position: Point(0, 0), id: .error)])
        .filter { $0.id == .block }
        .count
}

func runVideoGame(_ program: [Int]) -> Int {
    var program = program
    program[0] = 2 // free play!
    
    var screen: [Point: TileId] = [:]
    var nextPosition = Point(0, 0)
    var nextData = TileData.x
    var score = Int.min
    _ = runIntcodeProgram(ProgramState(
        memory: program,
        inputBlock: {
            printScreen(screen)
            print("score: \(score)")
            let paddle = screen.filter { _, v in v == .hPaddle }.keys.first!
            let ball = screen.filter { _, v in v == .ball }.keys.first!
            if ball.x < paddle.x { return -1 }
            else if ball.x == paddle.x { return 0 }
            else { return 1 }
        },
        outputBlock: { output in
            switch nextData {
            case .x: nextPosition.x = output
            case .y: nextPosition.y = output
            case .id:
                if nextPosition == Point(-1, 0) {
                    score = output
                } else {
                    let id = TileId(rawValue: output) ?? .error
                    screen[nextPosition] = id
                }
            }
            nextData = nextData.next
        }))
    return score
}

func printScreen(_ screen: [Point: TileId]) {
    let minX = screen
        .keys
        .map { $0.x }
        .reduce(Int.max, min)
    let maxX = screen
        .keys
        .map { $0.x }
        .reduce(Int.min, max)
    let minY = screen
        .keys
        .map { $0.y }
        .reduce(Int.max, min)
    let maxY = screen
        .keys
        .map { $0.y }
        .reduce(Int.min, max)
    print((minY...maxY)
        .map { y in
            (minX...maxX)
                .map { x in
                    switch screen[Point(x, y)] {
                    case nil, .empty: return " "
                    case .wall: return "#"
                    case .block: return "B"
                    case .hPaddle: return "="
                    case .ball: return "O"
                    case .error: return "E"
                    }
            }
            .joined()
    }
    .joined(separator: "\n"))
}
