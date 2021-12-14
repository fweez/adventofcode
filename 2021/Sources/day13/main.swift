import AOCShared
import Foundation
import Overture
import Parsing

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

enum Fold {
    case x(Int)
    case y(Int)
}

typealias ParsedStructure = (Set<Point>, [Fold])

let point = Int.parser()
    .skip(",")
    .take(Int.parser())
    .map { Point($0, $1) }
let points = Many(point, separator: "\n")
    .map { Set($0) }
let fold = OneOfMany(
    StartsWith("fold along x").map { "x" },
    StartsWith("fold along y").map { "y" })
    .skip("=")
    .take(Int.parser())
    .map { s, v -> Fold in
        switch s {
        case "x": return .x(v)
        case "y": return .y(v)
        default: fatalError()
        }
    }
let folds = Many(fold, separator: "\n")
let parser: AnyParser<Substring, ParsedStructure> = points
    .skip("\n\n")
    .take(folds)
    .eraseToAnyParser()
run(input: input, parser, part1, part2)
// or
//public func parse(_ input: String) -> ParsedStructure { 1 }
//run(input: input, parse, part1, part2)

func doFold(_ fold: Fold, input: Set<Point>) -> Set<Point> {
    input.reduce(into: Set<Point>()) { accum, p in
        switch fold {
        case .x(let foldPoint):
            if p.x < foldPoint {
                accum.insert(p)
            } else {
                let new = Point(foldPoint - (p.x - foldPoint), p.y)
                accum.insert(new)
            }
        case .y(let foldPoint):
            if p.y < foldPoint { accum.insert(p) }
            else {
                let new = Point(p.x, foldPoint - (p.y - foldPoint))
                accum.insert(new)
            }
        }
    }
}

func part1(_ parsedInput: ParsedStructure) {
    print("\(parsedInput.0.count) points")
    print("\(parsedInput.1.count) folds")
    
    let fold = parsedInput.1.first!
    let ps = doFold(fold, input: parsedInput.0)
    let a = ps.count
    print("Part 1: \(a)")
    
}

func viz(_ input: Set<Point>) -> String {
    (0...8).map { y in
        (0...42).map { x in
            let curr = Point(x, y)
            if input.contains(curr) {
                return "#"
            } else {
                return "."
            }
        }
        .joined()
    }
    .joined(separator: "\n")
}

func part2(_ parsedInput: ParsedStructure) {
    let final = parsedInput.1.reduce(parsedInput.0) { input, fold in
        doFold(fold, input: input)
    }
    print(viz(final))
    print("Part 2: n/a")
    
}
