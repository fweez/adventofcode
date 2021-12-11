import AOCShared
import Foundation
import Overture
import Parsing

let input = """
4525436417
1851242553
5421435521
8431325447
4517438332
3521262111
3331541734
4351836641
2753881442
7717616863
"""

typealias ParsedStructure = [[Int]]

func parse(_ input: String) -> ParsedStructure {
    input
        .split(separator: "\n")
        .map { $0.map(pipe(String.init, Int.init, force)) }
}

run(input: input, parse, part1, part2)

@discardableResult
func eachPosition<A>(in input: ParsedStructure, _ f: (Point) -> A) -> [[A]] {
    (0..<input.count).map { y in
        (0..<input.first!.count).map { x in
            return f(Point(x,y))
        }
    }
}

func incrementAll(_ input: inout ParsedStructure) {
    eachPosition(in: input) { p in input[p.y][p.x] = input[p.y][p.x] + 1 }
}

func neighbors(_ p: Point, input: ParsedStructure) -> [Point] {
    let maxX = input.first!.count - 1
    let maxY = input.count - 1
    
    return Array(
        (max(0, p.y - 1)...(min(maxY, p.y + 1)))
            .map { y in
                (max(0, p.x - 1)...(min(maxX, p.x + 1)))
                    .compactMap { x -> Point? in
                        if y == p.y && x == p.x { return nil }
                        return Point(x, y)
                    }
            }
            .joined())
    
}

func flash(_ input: inout ParsedStructure) -> Int {
    let count = eachPosition(in: input) { p -> Int in
        guard input[p.y][p.x] == 10 else { return 0 }
        neighbors(p, input: input)
            .forEach { n in
                guard input[n.y][n.x] < 10,
                      input[n.y][n.x] > 0
                else { return }
                input[n.y][n.x] = input[n.y][n.x] + 1
            }
        input[p.y][p.x] = 0
        return 1
    }
        .joined()
        .reduce(0, +)
    
    if count > 0 {
        return count + flash(&input)
    } else {
        return count
    }
}

func step(_ input: inout ParsedStructure) -> Int {
    incrementAll(&input)
    return flash(&input)
}

func part1(_ parsedInput: ParsedStructure) {
    var parsedInput = parsedInput
    let a = (0..<100)
        .map { _ in step(&parsedInput) }
        .reduce(0, +)
    print("Part 1: \(a)")
}

func allZeros(_ parsedInput: ParsedStructure) -> Bool {
    eachPosition(in: parsedInput) { p in parsedInput[p.y][p.x] }
    .allSatisfy { row in row.allSatisfy { $0 == 0 } }
}

func part2(_ parsedInput: ParsedStructure) {
    var stepCount = 0
    var parsedInput = parsedInput
    while allZeros(parsedInput) == false {
        step(&parsedInput)
        stepCount += 1
    }
    print("Part 2: \(stepCount)")
}
