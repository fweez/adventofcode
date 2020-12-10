import AOCShared
import Foundation
import Overture

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

var knownPaths: [Int: Int] = [:]

run(input: input, parse, part1, part2)

public typealias ParsedStructure = [Int]

public func parse(_ input: String) -> ParsedStructure {
    input
        .split(separator: "\n")
        .compactMap(pipe(String.init, Int.init))
}

public func part1(_ parsedInput: ParsedStructure) {
    let counts = parsedInput
        .sorted()
        .reduce((last: 0, ones: 0, threes: 0)) { t, curr in
            if curr - t.last == 1 { return (curr, t.ones + 1, t.threes) }
            else if curr - t.last == 3 { return (curr, t.ones, t.threes + 1) }
            else { return (curr, t.ones, t.threes) }
        }
    let result = counts.ones * (counts.threes + 1)
    print("Part 1: \(result)")
}

func pathsToEnd(from: Int, to: Int, _ adapters: Set<Int>) -> Int {
    if from == to { return 1 }
    guard adapters.contains(from) else { return 0 }
    if let v = knownPaths[from] { return v }
    let v = (1...3)
        .map { pathsToEnd(from: from + $0, to: to, adapters) }
        .reduce(0, +)
    knownPaths[from] = v
    return v
}

public func part2(_ parsedInput: ParsedStructure) {
    let paths = pathsToEnd(from: 0, to: parsedInput.max().map { $0 + 3 } ?? -99, Set(parsedInput + [0]))
    print("Part 2: \(paths)") }

