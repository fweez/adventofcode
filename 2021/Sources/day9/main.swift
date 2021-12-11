import AOCShared
import Foundation
import Overture
import Parsing

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

public typealias ParsedStructure = [[Int]]

public func parse(_ input: String) -> ParsedStructure {
    input.split(separator: "\n").map { s in s.map { c in pipe(String.init, Int.init)(c)! } }
}

run(input: input, parse, part1, part2)

func neighbors(_ p: Point, maxX: Int, maxY: Int) -> [Point] {
    var out: [Point] = []
    if p.y > 0 { out.append(Point(p.x, p.y - 1)) }
    if p.x < maxX { out.append(Point(p.x + 1, p.y)) }
    if p.y < maxY { out.append(Point(p.x, p.y + 1)) }
    if p.x > 0 { out.append(Point(p.x - 1, p.y)) }
    return out
}

func p1(_ input: ParsedStructure) -> Int {
    let maxX = input.first!.count - 1
    let maxY = input.count - 1
    return input
        .enumerated()
        .map { y, row in
            row
                .enumerated()
                .filter { x, val in
                    neighbors(Point(x, y), maxX: maxX, maxY: maxY)
                        .filter { input[$0.y][$0.x] <= val }
                        .count == 0
                }
        }
        .joined()
        .reduce(0) { accum, t in accum + t.element + 1}
}

public func part1(_ parsedInput: ParsedStructure) {
    let a = p1(parsedInput)
    print("Part 1: \(a)")
}

func viz(_ input: ParsedStructure) {
    print(input.map { $0.map(String.init).joined() }.joined(separator: "\n"))
}

func p2(_ input: ParsedStructure) -> Int {
    let maxX = input.first!.count - 1
    let maxY = input.count - 1
    var all = Set((0..<input.first!.count).map { x in (0..<input.count).map { y in Point(x, y) } }.joined())
    var binId = 10
    var input = input
    while all.count > 0 {
        var toVisit = [all.removeFirst()]
        while toVisit.count > 0 {
            let curr = toVisit.removeFirst()
            if input[curr.y][curr.x] == 9 { continue }
            input[curr.y][curr.x] = binId
            let unseenNeighbors = neighbors(curr, maxX: maxX, maxY: maxY).filter(all.contains)
            unseenNeighbors.forEach { _ = all.remove($0) }
            toVisit.append(contentsOf: unseenNeighbors)
        }
        binId += 1
    }
    
    return input
        .reduce(into: [Int: Int]()) { accum, row in
            accum = row.reduce(into: accum) { accum, v in accum[v] = (accum[v] ?? 0) + 1 }
        }
        .filter { k, _ in k != 9 }
        .values
        .sorted()
        .suffix(3)
        .reduce(1, *)
}

public func part2(_ parsedInput: ParsedStructure) {
    let a = p2(parsedInput)
    if a <= 897156 { print("Wrong answer") }
    if a >= 30303936 { print("Wrong answer") }
    print("Part 2: \(a)")
}

