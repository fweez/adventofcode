import AOCShared
import Foundation
import Overture

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

run(input: input, parse, part1, part2)

public typealias ParsedStructure = Int

public func parse(_ input: String) -> ParsedStructure { 1 }
public func part1(_ parsedInput: ParsedStructure) { print("Part 1: ") }
public func part2(_ parsedInput: ParsedStructure) { print("Part 2: ") }

