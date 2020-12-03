import AOCShared
import Foundation
import Overture

public func parse(_ input: String) -> [[Bool]] {
    ingestFile(input)
        .map { $0.map { $0 == "#" } }
}

func countTrees(_ pattern: [[Bool]], slope: Point) -> Int {
    guard let w = pattern.first?.count else { fatalError("No first line in pattern!") }
    return pattern
        .reduce((sum: 0, position: Point(0, 1))) { accum, line in
            var sum = accum.sum
            var position = accum.position
            position.y -= 1
            guard position.y == 0 else { return (sum, position) }
            if line[position.x % w] { sum += 1 }
            return (sum, position + slope)
        }
        .sum
}

func multiSlope(_ pattern: [[Bool]]) -> Int {
    [(1,1), (3,1), (5,1), (7,1), (1,2)]
        .map(pipe(Point.init, curry(countTrees)(pattern)))
        .reduce(1, *)
}

public func part1(_ parsedInput: [[Bool]]) { print("Part 1: \(countTrees(parsedInput, slope: Point(3,1)))") }
public func part2(_ parsedInput: [[Bool]]) { print("Part 2: \(multiSlope(parsedInput))") }
