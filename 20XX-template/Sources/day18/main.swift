import AOCShared
import Foundation
import Overture
import Parsing

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

typealias ParsedStructure = Int

let parser: AnyParser<Substring, ParsedStructure> = Int.parser().eraseToAnyParser()
run(input: input, parser, part1, part2)
// or
//func parse(_ input: String) -> ParsedStructure { 1 }
//run(input: input, parse, part1, part2)

func part1(_ parsedInput: ParsedStructure) { print("Part 1: ") }
func part2(_ parsedInput: ParsedStructure) { print("Part 2: ") }
