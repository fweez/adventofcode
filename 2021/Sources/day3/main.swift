import AOCShared
import Foundation
import Overture
import Parsing

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

public typealias ParsedStructure = [[Int]]

let t = "1".map { 1 }
let f = "0".map { 0 }
let bit = OneOfMany([t, f])
let line = Many(bit)
let parser: AnyParser<Substring, ParsedStructure> = Many(line, separator: "\n")
    .eraseToAnyParser()
run(input: input, parser, part1, part2)

public func part1(_ parsedInput: ParsedStructure) {
    let sums = parsedInput
        .reduce(Array(repeating: 0, count: parsedInput.first!.count)) { accum, curr in
            guard curr.count > 0 else { return accum }
            return zip(accum, curr).map {
                $0 + $1
            }
        }
    let cmp = parsedInput.count / 2
    let initial: (gamma: Int, epsilon: Int) = (0, 0)
    let (gamma, epsilon) = sums
        .reduce(initial) { accum, curr in
            if curr > cmp { return ((accum.0 << 1) + 1, accum.1 << 1) }
            else { return (accum.0 << 1, (accum.1 << 1) + 1) }
        }
    print("Part 1: \(gamma * epsilon)")
}

func getNext(_ arr: [[Int]], idx: Int, cmp: (Double, Double) -> Bool, tie: Int) -> [[Int]] {
    if arr.count == 1 { return arr }
    let sum = Double(arr
        .map { $0[idx] }
        .reduce(0, +))
    
    let bit: Int
    if sum == (Double(arr.count) / 2.0) { bit = tie }
    else if cmp(sum, (Double(arr.count) / 2.0)) { bit = 1 }
    else { bit = 0 }
    
    return arr.filter { $0[idx] == bit }
}

func int(_ arr: [Int]) -> Int {
    arr.reduce(0) { accum, i in
        if i == 0 { return accum << 1 }
        if i == 1 { return (accum << 1) + 1}
        fatalError("You fucked up")
    }
}

func getO2CO(_ parsedInput: ParsedStructure) -> (Int, Int) {
    let filtered = parsedInput.filter { $0.count > 0 }
    let (ox, co) = (0..<filtered.first!.count)
        .reduce((filtered, filtered)) { accum, idx in
            let nextOx = getNext(accum.0, idx: idx, cmp: >, tie: 1)
            let nextCo = getNext(accum.1, idx: idx, cmp: <, tie: 0)
            return (nextOx, nextCo)
        }
    
    guard let o = ox.first,
          let c = co.first
    else { fatalError("You fucked up: ox count \(ox.count), co count \(co.count)") }
    return (int(o), int(c))
}

public func part2(_ parsedInput: ParsedStructure) {
    let (o, c) = getO2CO(parsedInput)
    print("Part 2: \(o * c)")
}

public func testPart2() {
    let input = """
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
"""
    guard let parsed = parser.parse(input) else { fatalError("bad parser") }
    print(parsed)
    let (o, c) = getO2CO(parsed)
    assert(o == 23, "02 should be 23, got \(o)")
    assert(c == 10, "C02 should be 10, got \(c)")
}
