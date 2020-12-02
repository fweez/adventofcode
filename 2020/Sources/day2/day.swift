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

func verifyPart2(policy: Policy, password: String) -> Bool {
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
        characterParser)
    .map {
        Policy(required: $0.0...$0.2, char: $0.4)
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
        .filter(verifyPart1)
        .count
    print("Part 1: \(validPasswords)") }

public func part2(_ parsedInput: [(Policy, String)]) {
    let validPasswords = parsedInput
        .filter(verifyPart2)
        .count
    print("Part 2: \(validPasswords)")
}
