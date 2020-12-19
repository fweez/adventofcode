import AOCShared
import Foundation
import Overture

let rules = """
departure location: 32-615 or 626-955
departure station: 47-439 or 454-961
departure platform: 31-98 or 119-969
departure track: 45-746 or 763-967
departure date: 49-723 or 736-954
departure time: 42-556 or 581-962
arrival location: 46-401 or 418-964
arrival station: 39-281 or 295-974
arrival platform: 43-80 or 99-950
arrival track: 28-670 or 682-959
class: 43-504 or 520-957
duration: 31-358 or 365-959
price: 41-626 or 650-956
route: 26-488 or 495-949
row: 46-913 or 931-965
seat: 40-223 or 249-958
train: 32-832 or 853-966
type: 36-776 or 798-960
wagon: 38-122 or 134-969
zone: 27-870 or 885-952
"""

let myTicket = "191,61,149,157,79,197,67,139,59,71,163,53,73,137,167,173,193,151,181,179"

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

run(input: input, parse, part1, part2)

typealias ParsedStructure = (rules: [String: [ClosedRange<Int>]], mine: [Int], scanned: [[Int]])

func parse(_ input: String) -> ParsedStructure {
    let rangeParser = zip(intParser, literal("-"), intParser)
        .map { a, _, b in (a...b) }
    let ranges = zeroOrMore(rangeParser, separatedBy: literal(" or "))
    let ruleParser = zip(zeroOrMore(oneOf([alphaParser, literal(" ", " ")])), literal(": "), ranges)
        .map { k, _, v in (String(k.joined()), v) }
    let myRules = rules
        .split(separator: "\n")
        .compactMap(pipe(String.init, ruleParser.runStatic))
    let ticketParser: (Substring) -> [Int] = { $0.split(separator: ",").compactMap(pipe(String.init, Int.init))}
    let rules = Dictionary(uniqueKeysWithValues: myRules)
    let mine = ticketParser(myTicket[...])
    let scanned = input.split(separator: "\n").map(ticketParser)
    return (rules: rules,
            mine: mine,
            scanned: scanned)
}

func part1(_ parsedInput: ParsedStructure) {
    let invalid: (Int) -> Bool = { count in
        return parsedInput.rules.allSatisfy { k, ranges in
            ranges.contains { $0.contains(count) } == false
        }
    }
    let count = parsedInput.scanned
        .reduce([]) { invalidFields, ticket in
            invalidFields + ticket.filter(invalid)
        }
        .reduce(0, +)
    guard count < 2306324 else { fatalError() }
    print("Part 1: \(count)")
}

func matchingKeysFn(rules: [String: [ClosedRange<Int>]]) -> (Int) -> Set<String> {
    { count in
        Set(rules
                .filter { _, ranges in ranges.contains(where: { $0.contains(count) }) }
                .map { k, _ in k })
    }
}

func winnow(_ matches: [Set<String>]) -> [String] {
    let cuts = Set(matches
        .filter { $0.count == 1 }
        .map { $0.first! })
    if cuts.count == matches.count {
        return matches
            .map { $0.first! }
    }
    let filtered = matches.map { s -> Set<String> in
        if s.count == 1 { return s }
        return s.filter { cuts.contains($0) == false }
    }
    return winnow(filtered)
}

func keyMatches(_ parsedInput: ParsedStructure) -> [String] {
    let valid: (Int) -> Bool = { count in
        parsedInput.rules
            .contains(where: { _, ranges in ranges.contains { $0.contains(count) } })
    }
    let matching = matchingKeysFn(rules: parsedInput.rules)
    let possibleMatches = parsedInput.scanned
        .filter { $0.allSatisfy(valid) }
        .reduce(Array(repeating: Set(parsedInput.rules.keys), count: 20)) { accum, ticket in
            zip(accum, ticket)
                .map { prevMatches, count in
                    prevMatches.intersection(matching(count))
                }
        }
    
    return winnow(possibleMatches)
}

func part2(_ parsedInput: ParsedStructure) {
    testp2()
    let orderedKeys = keyMatches(parsedInput)
    let count = zip(orderedKeys, parsedInput.mine)
        .filter { k, _ in k.contains("departure") }
        .reduce(1) { $0 * $1.1 }
    print("Part 2: \(count)")
}

func testp2() {
    let input = (
        rules: [
            "class": [(0...1), (4...19)],
            "row": [(0...5), (8...19)],
            "seat": [(0...13), (16...19)]
        ],
        mine: [11, 12, 13],
        scanned: [
            [3,9,18],
            [15,1,5],
            [5,14,9]
        ])
    assert(keyMatches(input) == ["row", "class", "seat"])
}
