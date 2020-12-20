import AOCShared
import Foundation
import Overture

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}


enum Rule {
    case ref([Int])
    case `or`([Int], [Int])
    case char(Character)
}

typealias ParsedStructure = (rules: [Int: Rule], messages: [Substring])

let keyParser = zip(intParser, literal(": ")).map { i, _ in i }
let arrIntParser = zeroOrMore(intParser, separatedBy: literal(" "))
let refRulePaser = arrIntParser
    .map(Rule.ref)
let orRuleParser = zip(arrIntParser, literal(" | "), arrIntParser)
    .map { a, _, b in Rule.or(a, b) }
let charRuleParser = zip(literal("\""), characterParser, literal("\""))
    .map { _, c, _ in Rule.char(c) }
let ruleParser = zip(
    keyParser,
    oneOf([charRuleParser, orRuleParser, refRulePaser]))

func parseRules(_ input: Substring) -> (Int, Rule)? {
    var s = input
    return ruleParser.run(&s)
}

func parse(_ input: String) -> ParsedStructure {
    input.split(separator:"\n")
        .reduce(into: (rules: [:], messages: [])) { accum, line in
            if let (k, v) = parseRules(line) {
                accum.rules[k] = v
            } else if line.isEmpty != true {
                accum.messages.append(line)
            }
        }
}

func check(references: [Int], in rules: [Int: Rule], _ s: Substring, at index: Int) -> (Bool, Int) {
    var index = index
    let pass = references.allSatisfy { rule in
        let (pass, newIndex) = check(rule: rule, in: rules, s, at: index)
        index = newIndex
        return pass
    }
    return (pass, index)
}

func check(rule: Int, in rules: [Int: Rule], _ s: Substring, at index: Int) -> (Bool, Int) {
    switch rules[rule]! {
    case .ref(let references): return check(references: references, in: rules, s, at: index)
    case let .or(refA, refB):
        let (passA, indexA) = check(references: refA, in: rules, s, at: index)
        if passA { return (passA, indexA) }
        return check(references: refB, in: rules, s, at: index)
    case .char(let c):
        let idx = s.index(s.startIndex, offsetBy: index)
        guard s.endIndex > idx else { return (false, index) }
        return (s[idx] == c, index + 1)
    }
}

func matchFn(_ rules: [Int: Rule]) -> (Substring) -> Bool {
    { s in
        let (pass, index) = check(rule: 0, in: rules, s, at: 0)
        return index == s.count && pass
    }
}

func part1(_ parsedInput: ParsedStructure) {
    let matchesRules = matchFn(parsedInput.rules)
    let matches = parsedInput.messages
        .filter(matchesRules)
    print("Part 1: \(matches.count)")
}

func part2(_ parsedInput: ParsedStructure) {
    var modifiedRules = parsedInput.rules
    modifiedRules[8] = .or([42], [42, 8])
    modifiedRules[11] = .or([42, 31], [42, 11, 31])
    let matchesRules = matchFn(modifiedRules)
    let matches = parsedInput.messages
        .filter(matchesRules)
    let count = matches.count
    guard count > 252 else { fatalError("prev guess") }
    print("Part 2: \(count)") }

run(input: input, parse, part1, part2)
