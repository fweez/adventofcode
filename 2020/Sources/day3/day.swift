import AOCShared
import Foundation
import Overture

public func parse(_ input: String) -> [[Bool]] {
    ingestFile(input)
        .map { $0.map { $0 == "#" } }
}

func countTrees(_ pattern: [[Bool]], slope: (x: Int, y: Int)) -> Int {
    guard let w = pattern.first?.count else { fatalError("No first line in pattern!") }
    var x = 0
    var y = 1
    return pattern
        .filter { line in
            y -= 1
            guard y == 0 else { return false }
            let v = line[x % w]
            x += slope.x
            y += slope.y
            return v
        }
        .count
}

func multiSlope(_ pattern: [[Bool]]) -> Int {
    let f = curry(countTrees)(pattern)
    return f((1,1)) *
        f((3,1)) *
        f((5,1)) *
        f((7,1)) *
        f((1,2))
}

public func part1(_ parsedInput: [[Bool]]) { print("Part 1: \(countTrees(parsedInput, slope: (3,1)))") }
public func part2(_ parsedInput: [[Bool]]) { print("Part 2: \(multiSlope(parsedInput))") }
