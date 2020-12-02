import AOCShared
import Foundation
import Overture

public struct Policy {
    let required: ClosedRange<Int>
    let char: Character
}

func verifyPart1(policy: Policy, password: String) -> Bool {
    policy.required
        .contains(
            password
                .filter { $0 == policy.char }
                .count)
}

func verifyPart2(policy: Policy, password: Array<Character>) -> Bool {
    [password[(policy.required.first ?? Int.max) - 1],
     password[(policy.required.last ?? Int.max) - 1]]
        .filter { $0 == policy.char }
        .count == 1
}

let policy: Parser<Policy, String> =
    zip(
        intParser,
        literal("-"),
        intParser,
        literal(" "),
        alphaParser)
    .map {
        Policy(required: $0.0...$0.2, char: $0.4.first ?? "1")
    }

let policyAndPassword: Parser<(Policy, String), String> =
    zip(
        policy,
        literal(": "),
        alphaParser)
    .map { ($0.0, String($0.2)) }

public func parse(_ input: String) -> [(policy: Policy, password: String)] {
    parseFile(policyAndPassword, input)
}

public func part1(_ parsedInput: [(Policy, String)]) {
    let validPasswords = parsedInput
        .map(verifyPart1)
        .filter { $0 }
        .count
    print("Part 1: \(validPasswords)") }

public func part2(_ parsedInput: [(Policy, String)]) {
    let validPasswords = parsedInput
        .map { ($0.0, Array($0.1)) }
        .map(verifyPart2)
        .filter { $0 }
        .count
    print("Part 2: \(validPasswords)")
}
