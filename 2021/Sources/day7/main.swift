import AOCShared
import Foundation
import Overture
import Parsing
import Algorithms

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

public typealias ParsedStructure = [Int]

let parser: AnyParser<Substring, ParsedStructure> = Many(Int.parser(), separator: ",").eraseToAnyParser()
run(input: input, parser, part1, part2)

func getMin(_ parsedInput: ParsedStructure, cost: @escaping (Int) -> Int) -> Int {
    (0...parsedInput.max()!)
        .lazy
        .map { p in parsedInput.reduce(0) { a, v in a + cost(abs(v - p)) } }
        .adjacentPairs()
        .first { a, b in a < b }!
        .0
}

func p1(_ parsedInput: ParsedStructure) -> Int { getMin(parsedInput) { $0 } }

public func part1(_ parsedInput: ParsedStructure) {
    let a = p1(parsedInput)
    assert(a != 371, "previous guess")
    print("Part 1: \(a)")
}

func p2(_ parsedInput: ParsedStructure) -> Int { getMin(parsedInput) { n in (n * (n + 1)) / 2 } }

public func part2(_ parsedInput: ParsedStructure) {
    let a = p2(parsedInput)
    if a == 341558 { print("P2 WRONG ANSWER") }
    print("Part 2: \(a)")
}

func testp1() {
    let a = [16,1,2,0,4,2,7,1,2,14]
    let answer = p1(a)
    assert(answer == 37)
}

testp1()

func testp2() {
    let a = [16,1,2,0,4,2,7,1,2,14]
    let answer = p2(a)
    assert(answer == 168)
}

testp2()
