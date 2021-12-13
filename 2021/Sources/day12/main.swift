import AOCShared
import Foundation
import Overture
import Parsing

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

enum Cave: Equatable, Hashable {
    case little(String)
    case big(String)
    
    static func == (lhs: Cave, rhs: Cave) -> Bool {
        switch (lhs, rhs) {
        case let (.big(a), .big(b)), let (.little(a), .little(b)): return a == b
        default: return false
        }
    }
}

typealias ParsedStructure = [(Cave, Cave)]

let cave = CharacterSet.letters
    .map { s -> Cave in
        if s.unicodeScalars.allSatisfy(CharacterSet.lowercaseLetters.contains) {
            return Cave.little(String(s))
        } else {
            return Cave.big(String(s))
        }
    }
let path = cave
    .skip("-")
    .take(cave)
let parser: AnyParser<Substring, ParsedStructure> = Many(path, separator: "\n")
    .eraseToAnyParser()
run(input: input, parser, part1, part2)
// or
//public func parse(_ input: String) -> ParsedStructure { 1 }
//run(input: input, parse, part1, part2)

func adjoining(from: Cave, links: ParsedStructure) -> [Cave] {
    let adj = links.reduce([] as [Cave]) { accum, link in
        if link.0 == from { return accum + [link.1] }
        if link.1 == from { return accum + [link.0] }
        return accum
    }
    return adj
    
}

func paths(from: Cave, links: ParsedStructure) -> [[Cave]] {
    switch from {
    case .little("end"): return [[from]]
    case .big:
        let a = Array(adjoining(from: from, links: links)
                        .map { paths(from: $0, links: links) }
                        .joined()
                        .map { [from] + $0 }
        )
        return a
    case .little:
        let filtered = links.filter { $0.0 != from && $0.1 != from }
        let a = Array(adjoining(from: from, links: links)
                        .map { paths(from: $0, links: filtered) }
                        .joined()
                        .map { [from] + $0 }
        )
        return a
    }
}

func part1(_ parsedInput: ParsedStructure) {
    let start = Cave.little("start")
    let starts = adjoining(from: start, links: parsedInput)
    let remaining = parsedInput.filter { $0.0 != start && $0.1 != start }
    let a = starts
        .map { paths(from: $0, links: remaining) }
        .map(\.count)
        .reduce(0, +)
    guard a == 5212 else { print("You messed up p1 \(a)"); return }
    print("Part 1: \(a)")
    
}

func p2Paths(from: Cave, links: ParsedStructure, pathToHere: [Cave]) -> [[Cave]] {
//    print("from \(from)")
//    print("links \(links)")
//    print("path \(pathToHere)")
    if from == .little("end") { return [[from]] }
    
    let littles = pathToHere.filter {
        switch $0 {
        case .little: return true
        case .big: return false
        }
    }
    let sLittles = Set(littles)
    let filtered: ParsedStructure
    if sLittles.count < littles.count {
        filtered = links.filter { (sLittles.contains($0.0) || sLittles.contains($0.1)) == false }
    } else {
        filtered = links
    }
//    print("filtered \(filtered)")
    let a = Array(adjoining(from: from, links: filtered)
                    .map { p2Paths(
                        from: $0,
                        links: filtered,
                        pathToHere: pathToHere + [from]) }
                    .joined()
                    .map { [from] + $0 }
    )
    return a
}

func p2Paths(_ parsedInput: ParsedStructure) -> [[Cave]] {
    let start = Cave.little("start")
    let starts = adjoining(from: start, links: parsedInput)
    let remaining = parsedInput.filter { $0.0 != start && $0.1 != start }
    return Array(starts
                    .map { p2Paths(from: $0, links: remaining, pathToHere: []) }
        .joined())
}

func part2(_ parsedInput: ParsedStructure) {
    let allPaths = p2Paths(parsedInput)
    let a = allPaths.count
    guard a > 35974 else { print("WRONG ANSWER \(a)"); return }
    print("Part 1: \(a)")
}

func testp2() {
    let input = """
start-A
start-b
A-c
A-b
b-d
A-end
b-end
"""
    let expected = """
A,b,A,b,A,c,A,end
A,b,A,b,A,end
A,b,A,b,end
A,b,A,c,A,b,A,end
A,b,A,c,A,b,end
A,b,A,c,A,c,A,end
A,b,A,c,A,end
A,b,A,end
A,b,d,b,A,c,A,end
A,b,d,b,A,end
A,b,d,b,end
A,b,end
A,c,A,b,A,b,A,end
A,c,A,b,A,b,end
A,c,A,b,A,c,A,end
A,c,A,b,A,end
A,c,A,b,d,b,A,end
A,c,A,b,d,b,end
A,c,A,b,end
A,c,A,c,A,b,A,end
A,c,A,c,A,b,end
A,c,A,c,A,end
A,c,A,end
A,end
b,A,b,A,c,A,end
b,A,b,A,end
b,A,b,end
b,A,c,A,b,A,end
b,A,c,A,b,end
b,A,c,A,c,A,end
b,A,c,A,end
b,A,end
b,d,b,A,c,A,end
b,d,b,A,end
b,d,b,end
b,end
"""
    guard let pi = parser.parse(input) else { fatalError() }
    let ps = p2Paths(pi)
    let sps = ps.map {
        $0
            .map { c -> String in
                switch c {
                case .little(let s), .big(let s): return s
                }
            }
            .joined(separator: ",")
    }
    zip(expected.split(separator: "\n").sorted(), sps.sorted())
        .forEach { assert($0 == $1, "\nexp \($0)\ngot \($1)") }
}

testp2()
