import AOCShared
import Foundation
import Overture

public func parse(_ input: String) -> [[Int]] {
    ingestFile(input)
        .map { $0.map { $0 == "#" ? 1 : 0 } }
}

func countTrees(_ pattern: [[Int]], slope: Point) -> Int {
    pattern
        .reduce((sum: 0, position: Point(0, 1))) { accum, line in
            let position = accum.position + Point(0, -1)
            guard position.y == 0 else { return (accum.sum, position) }
            return (accum.sum + line[position.x % line.count], position + slope)
        }
        .sum
}

func multiSlope(_ pattern: [[Int]]) -> Int {
    [(1,1), (3,1), (5,1), (7,1), (1,2)]
        .map(pipe(Point.init, curry(countTrees)(pattern)))
        .reduce(1, *)
}

public func part1(_ parsedInput: [[Int]]) { print("Part 1: \(countTrees(parsedInput, slope: Point(3,1)))") }
public func part2(_ parsedInput: [[Int]]) { print("Part 2: \(multiSlope(parsedInput))") }
