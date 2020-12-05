import AOCShared
import Foundation
import Overture

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

run(input: input, parse, part1, part2)

public typealias ParsedStructure = [Int]

func seat(_ s: Substring) -> (row: Int, col: Int) {
    (s
        .prefix(7)
        .reduce((position: 0, length: 128)) { a, c in
            switch c {
            case "F": return (a.0, a.1 / 2)
            case "B": return (a.0 + a.1 / 2, a.1 / 2)
            default: fatalError("Unexpected character for row selection: \(c)")
            }
        }
        .position,
     s
        .suffix(3)
        .reduce((position: 0, length: 8)) { a, c in
            switch c {
            case "L": return (a.0, a.1 / 2)
            case "R": return (a.0 + a.1 / 2, a.1 / 2)
            default: fatalError("Unexpected character for row selection: \(c)")
            }
        }
        .position)
}

func seatId(row: Int, col: Int) -> Int { (row * 8) + col }

func missing(_ seats: [Int]) -> Int {
    (1..<(seats.count - 1))
        .first(where: { idx in seats[idx + 1] - 1 != seats[idx] })
        .map { seats[$0] +  1 }
        ?? Int.min
}

public func parse(_ input: String) -> ParsedStructure {
    input
        .split(separator: "\n")
        .map(pipe(seat, seatId))
        .sorted()
}

public func part1(_ parsedInput: ParsedStructure) { print("Part 1: \(parsedInput.last ?? Int.min)") }
public func part2(_ parsedInput: ParsedStructure) { print("Part 2: \(missing(parsedInput))") }

