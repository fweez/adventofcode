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
    input
        .split(separator: "\n")
        .compactMap(pipe(String.init, Int.init))
}

public func part1(_ parsedInput: ParsedStructure) {
    let out = parsedInput
        .reduce((Int.max, 0)) { accum, curr in
            if curr > accum.0 {
                return (curr, accum.1 + 1)
            } else {
                return (curr, accum.1)
            }
        }
        .1
    print("Part 1: \(out)")
}

public func part2(_ parsedInput: ParsedStructure) {
    let out = parsedInput
        .reduce((Int.max, Int.max, Int.max, 0)) { accum, curr in
            guard accum.0 != Int.max,
                  accum.1 != Int.max,
                  accum.2 != Int.max
            else { return (accum.1, accum.2, curr, accum.3) }
            let lastWindow = accum.0 + accum.1 + accum.2
            let currWindow = accum.1 + accum.2 + curr
            if lastWindow < currWindow {
                return (accum.1, accum.2, curr, accum.3 + 1)
            } else {
                return (accum.1, accum.2, curr, accum.3)
            }
        }
        .3
    print("Part 2: \(out)")
}

