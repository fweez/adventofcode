import AOCShared
import Foundation
import Overture

/// To start a new day, copy and paste this file into a new dayX directory in Sources/
public func part1() { print("Part 1: \(countPasswords(356261...846303))") }
public func part2() { print("Part 2: \(countPasswordsB(356261...846303))") }

func countPasswords(_ range: ClosedRange<Int>) -> Int {
    range
        .map(String.init)
        .filter(isIncreasing)
        .filter(hasDupe)
        .count
}

func countPasswordsB(_ range: ClosedRange<Int>) -> Int {
    range
        .map(String.init)
        .filter(isIncreasing)
        .filter(has2Dupe)
        .count
}

func isIncreasing(_ s: String) -> Bool {
    s.reduce("0") { last, curr -> Character in
        if last == "x" { return last }
        if curr < last { return "x" }
        return curr
    } != "x"
}

let dupeRegexes = "0123456789".map { ".*\($0)\($0).*" }
func hasDupe(_ s: String) -> Bool {
    for regex in dupeRegexes {
        if s.range(of: regex, options: .regularExpression) != nil { return true }
    }
    return false
}

let capturingDupeRegexes = "0123456789".map { "(\($0)+)" }
func has2Dupe(_ s: String) -> Bool {
    capturingDupeRegexes
        .map {
            guard let r = s.range(of: $0, options: .regularExpression) else { return 0 }
            return s[r].count
        }
    .filter { $0 == 2 }
    .count > 0
}
