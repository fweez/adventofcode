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
    [(1,1), (3,1), (5,1), (7,1), (1,2)]
        .map(curry(countTrees)(pattern))
        .reduce(1, *)
}

public func part1(_ parsedInput: [[Bool]]) { print("Part 1: \(countTrees(parsedInput, slope: (3,1)))") }
public func part2(_ parsedInput: [[Bool]]) { print("Part 2: \(multiSlope(parsedInput))") }
