import AOCShared
import Foundation
import Overture

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

run(input: input, parse, part1, part2)

public typealias ParsedStructure = [Int]

public func parse(_ input: String) -> ParsedStructure {
    input.split(separator: "\n").compactMap(pipe(String.init, Int.init))
}

var magicNumber: Int = Int.min

public func part1(_ parsedInput: ParsedStructure) {
    magicNumber = parsedInput
        .enumerated()
        .first { idx, i in
            if idx < 25 { return false }
            let prevChunk = Array(parsedInput.prefix(upTo: idx).suffix(25))
            return prevChunk
                .enumerated()
                .first { idx, a in
                    prevChunk
                        .suffix(from: idx + 1)
                        .first { b in a + b == i }
                        != nil
                }
                == nil
        }?
        .1 ?? Int.min
    
    print("Part 1: \(magicNumber)")
}

public func part2(_ parsedInput: ParsedStructure) {
    let range = (0..<(parsedInput.count - 2))
        .reduce((0,2)) { t, idxA -> (Int, Int) in
            let total = parsedInput[t.0..<t.1]
                .reduce(0, +)
            if total == magicNumber { return t }
            if total < magicNumber { return (t.0, t.1 + 1) }
            else { return (t.0 + 1, t.1) }
        }
    let chunk = parsedInput[range.0..<range.1]
        .sorted()
    let total = (chunk.first ?? Int.min) + (chunk.last ?? Int.min)
    print("Part 2: \(total)")
}
