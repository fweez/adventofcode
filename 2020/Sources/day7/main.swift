import AOCShared
import Foundation
import Overture

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

run(input: input, parse, part1, part2)

public typealias ParsedStructure = [Substring: [Substring: Int]]

public func parse(_ input: String) -> ParsedStructure {
    let bagDescription = zip(
        alphaParser,
        literal(" "),
        alphaParser,
        literal(" bag"),
        zeroOrMore(literal("s")))
        .map { a, _, b, _, _ in a + " " + b }
    let contained = zip(
        intParser,
        literal(" "),
        bagDescription)
        .map { count, _, color in (color, count) }
    let allContained = zeroOrMore(contained, separatedBy: literal(", "))
        .map(Dictionary.init(uniqueKeysWithValues:))
    let container = zip(
        bagDescription,
        literal(" "),
        alphaParser,
        literal(" "),
        allContained,
        literal(".\n"))
        .map { color, _, _, _, contained, _ in
            (color, contained)
        }    
    let emptyContainer = zip(
        bagDescription,
        literal(" contain no other bags.\n"))
        .map { color, _ in
            (color, Dictionary<Substring, Int>())
        }
    return zeroOrMore(oneOf([container, emptyContainer]))
        .map(Dictionary.init(uniqueKeysWithValues:))
        .runStatic(input)
        ?? [:]
}

func containsShinyGold(key: Substring, _ s: ParsedStructure) -> Bool {
    guard let children = s[key] else { fatalError() }
    if children.keys.contains("shiny gold") { return true }
    return children.keys.first(where: { containsShinyGold(key: $0, s) }) != nil
}

public func part1(_ parsedInput: ParsedStructure) {
    let count = parsedInput.keys
        .filter { containsShinyGold(key: $0, parsedInput) }
        .count
    print("Part 1: \(count)")
}

public func bagsIn(key: Substring, _ s: ParsedStructure) -> Int {
    s[key]?
        .reduce(s[key]?.values.reduce(0, +) ?? Int.min) {
            $0 + (bagsIn(key: $1.key, s) * $1.value)
        }
        ?? Int.min
}

public func part2(_ parsedInput: ParsedStructure) {
    print("Part 2: \(bagsIn(key: "shiny gold", parsedInput))")
}

