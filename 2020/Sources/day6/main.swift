import AOCShared
import Foundation
import Overture

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

run(input: input, parse, part1, part2)

public typealias ParsedStructure = [[Substring]]

public func parse(_ input: String) -> ParsedStructure {
    let group = zeroOrMore(alphaParser, separatedBy: literal("\n"))
    let allGroups = zeroOrMore(group, separatedBy: literal("\n\n"))
    return allGroups.runStatic(input) ?? []
}

public func part1(_ parsedInput: ParsedStructure) {
    let count = parsedInput
        .reduce([Set<Character>()]) { accum, answers in
            var s = Set<Character>()
            answers.forEach { $0.forEach { s.insert($0) } }
            return accum + [s]
        }
        .reduce(0) { a, g in a + g.count }
    print("Part 1: \(count)")
}

public func part2(_ parsedInput: ParsedStructure) {
    let count = parsedInput
        .reduce([Set<Character>()]) { accum, answers in
            guard answers.count >= 1 else { return accum }
            var answers = answers
            let first = answers.removeFirst()
            var s = Set<Character>(first)
            answers.forEach { s.formIntersection($0) }
            return accum + [s]
        }
        .reduce(0) { a, g in a + g.count }
    print("Part 2: \(count)")
}

