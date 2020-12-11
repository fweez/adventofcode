import AOCShared
import Foundation
import Overture

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

run(input: input, parse, part1, part2)

enum Position: Character {
    case floor = "."
    case empty = "L"
    case occupied = "#"
}

typealias ParsedStructure = [[Position]]


func countAdjacentPart1(f: [[Position]], y: Int, x: Int) -> Int {
    (max(0, y - 1)...min(f.count - 1, y + 1))
        .reduce(0) { (accum: Int, row: Int) -> Int in
            (max(0, x - 1)...min(f[row].count - 1, x + 1))
                .reduce(0) { (accum: Int, col: Int) -> Int in
                    if row == y && col == x { return accum }
                    else if f[row][col] == .occupied { return accum + 1 }
                    else { return accum }
                }
                + accum
        }
}

func part1Rules(f: [[Position]], y: Int, x: Int) -> Position {
    let curr = f[y][x]
    switch (curr, countAdjacentPart1(f: f, y: y, x: x)) {
    case (.floor, _): return .floor
    case (.empty, 0): return .occupied
    case (.occupied, let n) where n >= 4: return .empty
    default: return curr
    }
}

func intersectsOccupied(_ f: [[Position]], y: Int, x: Int, rise: Int, run: Int) -> Bool {
    guard y < f.count, y >= 0, x < f.first!.count, x >= 0 else { return false }
    switch f[y][x] {
    case .occupied: return true
    case .empty: return false
    case .floor: return intersectsOccupied(f, y: y + rise, x: x + run, rise: rise, run: run)
    }
}

func countAdjacentPart2(f: [[Position]], y: Int, x: Int) -> Int {
    (max(0, y - 1)...min(f.count - 1, y + 1))
        .reduce(0) { (accum: Int, row: Int) -> Int in
            (max(0, x - 1)...min(f[row].count - 1, x + 1))
                .reduce(0) { (accum: Int, col: Int) -> Int in
                    if row == y && col == x { return accum }
                    else if f[row][col] == .occupied { return accum + 1 }
                    else if f[row][col] == .floor,
                            intersectsOccupied(f, y: row, x: col, rise: row - y, run: col - x) {
                            return accum + 1
                    } else { return accum }
                }
                + accum
        }
}

func part2Rules(f: ParsedStructure, y: Int, x: Int) -> Position {
    let curr = f[y][x]
    switch (curr, countAdjacentPart2(f: f, y: y, x: x)) {
    case (.floor, _): return .floor
    case (.empty, 0): return .occupied
    case (.occupied, let n) where n >= 5: return .empty
    default: return curr
    }
}

func tick(_ f: [[Position]], rules: @escaping (ParsedStructure, Int, Int) -> Position) -> [[Position]] {
    let applyY = curry(rules)(f)
    return (0..<f.count)
        .map { y in
            let applyX = applyY(y)
            return (0..<f.first!.count)
                .map(applyX)
        }
}

func countOccupied(_ f: ParsedStructure) -> Int {
    f
        .reduce(0) {
            $0 + $1.reduce(0) {
                if $1 == .occupied { return 1 + $0 }
                else { return $0 }
            }
        }
}

func parse(_ input: String) -> ParsedStructure {
    input
        .split(separator: "\n")
        .map { $0.compactMap(Position.init(rawValue:)) }
}

func debug(_ f: ParsedStructure) { f.forEach { print(String($0.map { $0.rawValue })) } }

func runToSteadyState(_ f: ParsedStructure, rules: @escaping (ParsedStructure, Int, Int) -> Position) -> Int {
    var last = tick(f, rules: rules)
    while true {
        let new = tick(last, rules: rules)
        let count = countOccupied(new)
        if count == countOccupied(last) {
            return count
        } else {
            last = new
        }
    }
}

func part1(_ parsedInput: ParsedStructure) {
    print("Part 1: \(runToSteadyState(parsedInput, rules: part1Rules))")
}

func part2(_ parsedInput: ParsedStructure) {
    print("Part 2: \(runToSteadyState(parsedInput, rules: part2Rules))")
}

func testP1() {
    let testData = """
    L.LL.LL.LL
    LLLLLLL.LL
    L.L.L..L..
    LLLL.LL.LL
    L.LL.LL.LL
    L.LLLLL.LL
    ..L.L.....
    LLLLLLLLLL
    L.LLLLLL.L
    L.LLLLL.LL
    """
    let testParse = parse(testData)
    assert(runToSteadyState(testParse, rules: part1Rules) == 37)
}
func testP2() {
    let testData = """
    L.LL.LL.LL
    LLLLLLL.LL
    L.L.L..L..
    LLLL.LL.LL
    L.LL.LL.LL
    L.LLLLL.LL
    ..L.L.....
    LLLLLLLLLL
    L.LLLLLL.L
    L.LLLLL.LL
    """

    let testParse = parse(testData)
    let count = runToSteadyState(testParse, rules: part2Rules)
    assert(count == 26)
}
