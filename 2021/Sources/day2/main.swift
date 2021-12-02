import AOCShared
import Foundation
import Overture

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

run(input: input, parse, part1, part2)

public enum Direction {
    case forward(Int)
    case down(Int)
    case up(Int)
}

public typealias ParsedStructure = [Direction]

public func parse(_ input: String) -> ParsedStructure {
    input.split(separator: "\n")
        .map { line in
            let c = line.split(separator: " ")
            guard let s = c.last, let i = Int(s) else { fatalError() }
            switch c.first {
            case "forward": return .forward(i)
            case "down": return .down(i)
            case "up": return .up(i)
            default: fatalError()
            }
        }
}

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
