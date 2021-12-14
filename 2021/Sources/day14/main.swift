import AOCShared
import Foundation
import Overture
import Parsing
import Algorithms

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

typealias ParsedStructure = (Substring, [String: Substring])

let template = CharacterSet.uppercaseLetters.eraseToAnyParser()
let insertion = CharacterSet.uppercaseLetters
    .skip(" -> ")
    .take(CharacterSet.uppercaseLetters)
    .map { (String($0), $1) }
let insertions = Many(insertion, separator: "\n")
    .map { Dictionary<String, Substring>(uniqueKeysWithValues: $0) }
let parser: AnyParser<Substring, ParsedStructure> = template
    .skip("\n\n")
    .take(insertions)
    .eraseToAnyParser()

run(input: input, parser, part1, part2)

func step(input: String, insertions: [String: Substring]) -> String {
    input.reduce("") { accum, c in
        guard let l = accum.last else { return accum + "\(c)" }
        let k = "\(l)\(c)"
        if let i = insertions[k] {
            return accum + "\(i)\(c)"
        } else {
            return accum + "\(c)"
        }
    }
}

func runInsertions(
    step: Int,
    pair: String,
    insertions: [String: Substring],
    memo: inout [Int: [String: [Character: Int]]]
) -> [Character: Int] {
    let left = pair.first!
    let right = pair.last!
    guard step > 0,
          let insertion = insertions[pair]?.first
    else {
        // Base case -- ran out of steps or this pair doesn't have an insertion
        return [:]
    }
    if let memoized = memo[step]?[pair] {
        return memoized
    }
    var leftCounts = runInsertions(
        step: step - 1,
        pair: "\(left)\(insertion)",
        insertions: insertions,
        memo: &memo)
    let rightCounts = runInsertions(
        step: step - 1,
        pair: "\(insertion)\(right)",
        insertions: insertions,
        memo: &memo)
    leftCounts.merge(rightCounts, uniquingKeysWith: +)
    leftCounts[insertion] = (leftCounts[insertion] ?? 0) + 1
    if memo[step] == nil {
        memo[step] = [:]
    }
    memo[step]![pair] = leftCounts
//    print("step \(step) pair \(pair) inserted \(insertion): \(leftCounts)")
    return leftCounts
}

func origPart1(_ parsedInput: ParsedStructure) {
    let polymer = (0..<10)
        .reduce(String(parsedInput.0)) { accum, _ in
            return step(input: accum, insertions: parsedInput.1)
        }
    let d = parsedInput.1.values
        .reduce(into: [:] as [String: Int]) { accum, c in
            let s = String(c)
            accum[s] = polymer.filter { String($0) == s }.count
        }
    print(d)
    let s = d.values.sorted()
    let a = s.last! - s.first!
    print("Part 1: \(a)")
}

func runit(_ parsedInput: ParsedStructure, steps: Int) -> Int {
    var memo: [Int: [String: [Character: Int]]] = [:]
    var initial: [Character: Int] = [:]
    parsedInput.0.forEach { c in
        initial[c] = (initial[c] ?? 0) + 1
    }
    let counts = parsedInput.0
        .adjacentPairs()
        .reduce(into: initial) { accum, pair in
            let p = "\(pair.0)\(pair.1)"
            let new = runInsertions(step: steps, pair: p, insertions: parsedInput.1, memo: &memo)
            accum.merge(new, uniquingKeysWith: +)
        }
    let vs = counts.values.sorted()
    return vs.last! - vs.first!
}

func part1(_ parsedInput: ParsedStructure) {
    let a = runit(parsedInput, steps: 10)
    if a != 2375 { print("You fucked it up") }
    print("Part 1: \(a)")
}

func part2(_ parsedInput: ParsedStructure) {
    let a = runit(parsedInput, steps: 40)
    print("Part 2: \(a)")

}
