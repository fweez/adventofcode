import AOCShared
import Foundation
import Overture
import Parsing

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

public typealias ParsedStructure = [(Point, Point)]

let point = Int.parser()
    .skip(",")
    .take(Int.parser())
    .map { Point($0, $1) }
let line = point
    .skip(" -> ")
    .take(point)
let parser: AnyParser<Substring, ParsedStructure> = Many(line, separator: "\n")
    .eraseToAnyParser()
run(input: input, parser, part1, part2)
// or
//public func parse(_ input: String) -> ParsedStructure { 1 }
//run(input: input, parse, part1, part2)

func minmax(_ a: Int, _ b: Int) -> (Int, Int) { (min(a, b), max(a, b)) }

func addHVLine(accum: inout [Point: Int], line: (Point, Point)) {
    if line.0.x == line.1.x {
        let (start, end) = minmax(line.0.y, line.1.y)
        (start...end)
            .map { Point(line.0.x, $0) }
            .forEach { accum[$0] = (accum[$0] ?? 0) + 1 }
        return
    }
    let (start, end) = minmax(line.0.x, line.1.x)
    
    (start...end)
        .map { Point($0, line.0.y) }
        .forEach { accum[$0] = (accum[$0] ?? 0) + 1 }
}

func addDiag(accum: inout [Point: Int], line: (Point, Point)) {
    let xMod: Int
    if line.0.x > line.1.x { xMod = -1 }
    else { xMod = 1 }
    let yMod: Int
    if line.0.y > line.1.y { yMod = -1 }
    else { yMod = 1 }
    (0...abs(line.0.x - line.1.x)) // they're always 45 degree angles
        .map { idx in line.0 + Point(xMod * idx, yMod * idx) }
        .forEach { accum[$0] = (accum[$0] ?? 0) + 1 }
}

func getP1Map(_ input: ParsedStructure) -> [Point: Int] {
    input
        .filter { $0.0.x == $0.1.x || $0.0.y == $0.1.y }
        .reduce(into: [:] as [Point: Int], addHVLine)
}

func getP2Map(_ input: ParsedStructure) -> [Point: Int] {
    input
        .reduce(into: [:] as [Point: Int]) { accum, line in
            if line.0.x == line.1.x || line.0.y == line.1.y {
                addHVLine(accum: &accum, line: line)
            } else {
                addDiag(accum: &accum, line: line)
            }
        }
}

func countOverlaps(_ input: [Point: Int]) -> Int {
    input
        .filter { k, v in v >= 2 }
        .count
}

public func part1(_ parsedInput: ParsedStructure) {
    let map = getP1Map(parsedInput)
    let count = countOverlaps(map)
    assert(count != 901, "previous guess")
    print("Part 1: \(count)")

}
public func part2(_ parsedInput: ParsedStructure) {
    let map = getP2Map(parsedInput)
    let count = countOverlaps(map)
    assert(count > 8073, "previous guess")
    print("Part 2: \(count)")
}

let testInput = """
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
"""

func vizTest(_ map: [Point: Int]) {
    print(
        (0...10)
            .map { x in
                (0...10)
                    .map { y in map[Point(x, y)] }
                    .map { $0.map(String.init) ?? "." }
                    .joined()
            }
            .joined(separator: "\n"))
}

func testp1() {
    guard let parsedInput = parser.parse(testInput) else { fatalError() }
    let map = getP1Map(parsedInput)
    let count = countOverlaps(map)
    vizTest(map)
    assert(count == 5, "Got count \(count)")
}

func testP2() {
    guard let parsedInput = parser.parse(testInput) else { fatalError() }
    let map = getP2Map(parsedInput)
    let count = countOverlaps(map)
    vizTest(map)
    assert(count == 12, "Got count \(count)")
}

//testp1()
//testP2()
