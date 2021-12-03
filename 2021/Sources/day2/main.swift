import AOCShared
import Foundation
import Overture
import Parsing

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

public enum Direction {
    case forward(Int)
    case down(Int)
    case up(Int)
}

public typealias ParsedStructure = [Direction]

let forward = "forward "
    .take(Int.parser())
    .map(Direction.forward)
let down = "down "
    .take(Int.parser())
    .map(Direction.down)
let up = "up "
    .take(Int.parser())
    .map(Direction.up)
let dir = OneOfMany(forward, down, up)
let parser = Many(dir, separator: "\n").eraseToAnyParser()

run(input: input, parser, part1, part2)

public func part1(_ parsedInput: ParsedStructure) {
    let dest = parsedInput
        .reduce(Point(0, 0)) { accum, dir in
            switch dir {
            case .forward(let i): return accum + Point(i, 0)
            case .down(let i): return accum + Point(0, i)
            case .up(let i): return accum - Point(0, i)
            }
        }
    print("Part 1: \(dest.x * dest.y)")
    
}
public func part2(_ parsedInput: ParsedStructure) {
    let initial: (pos: Point, aim: Int) = (Point(0, 0), 0)
    let dest = parsedInput
        .reduce(initial) { accum, dir in
            switch dir {
            case .forward(let i):
                let nextPos = accum.pos + Point(i, 0) + Point(0, accum.aim * i)
                return (nextPos, accum.aim)
            case .down(let i):
                return (accum.pos, accum.aim + i)
            case .up(let i):
                return (accum.pos, accum.aim - i)
            }
        }
    print("Part 2: \(dest.pos.x * dest.pos.y)")
}
