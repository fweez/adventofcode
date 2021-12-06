import AOCShared
import Foundation
import Overture
import Parsing
import Collections

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

public typealias ParsedStructure = Deque<Int>

let parser: AnyParser<Substring, ParsedStructure> = Many(Int.parser(), separator: ",")
    .map { ages in
        var out = Array(repeating: 0, count: 9)
        ages.forEach { age in out[age] += 1 }
        return Deque(out)
    }
    .eraseToAnyParser()
run(input: input, parser, part1, part2)
// or
//public func parse(_ input: String) -> ParsedStructure { 1 }
//run(input: input, parse, part1, part2)

func growLanternfish(_ parsedInput: ParsedStructure, days: Int) -> Int {
    (0..<days)
        .reduce(into: parsedInput) { accum, day in
            let buddingToday = accum.removeFirst()
            accum[6] += buddingToday // budders reset
            accum.append(buddingToday) // 8th element, buds start growing
        }
        .reduce(0, +)
}

public func part1(_ parsedInput: ParsedStructure) {
    print("Part 1: \(growLanternfish(parsedInput, days: 80))")
}
public func part2(_ parsedInput: ParsedStructure) {
    print("Part 2: \(growLanternfish(parsedInput, days: 256))")
}
