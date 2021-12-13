import AOCShared
import Foundation
import Overture

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

typealias ParsedStructure = [String]

func parse(_ input: String) -> ParsedStructure {
    input
        .split(separator: "\n")
        .map { String($0) }
}
run(input: input, parse, part1, part2)

func part1(_ parsedInput: ParsedStructure) {
    let r = parsedInput
        .map { s in
            s
                .replacingOccurrences(
                    of: #"\\x[\dabcdef][\dabcdef]"#,
                    with: "0",
                    options: .regularExpression)
                .replacingOccurrences(of: #"\""#, with: #"""#)
                .replacingOccurrences(of: #"\\"#, with: "\\")
        }
    
    let inputSize = parsedInput.reduce(0) { a, s in a + s.count }
    let replaceSize = r.reduce(0) { a, s in a + s.count - 2 }
    
    let a = inputSize - replaceSize
    guard a > 1244 else { print("wrong answer"); return }
    print("Part 1: \(a)")
}
                                      
func part2(_ parsedInput: ParsedStructure) {
    let r = parsedInput
        .map { s in
            s
                .replacingOccurrences(of: "\\", with: "\\\\")
                .replacingOccurrences(of: "\"", with: "\\\"")
        }
    
    let inputSize = parsedInput.reduce(0) { a, s in a + s.count }
    let replaceSize = r.reduce(0) { a, s in a + s.count - 2 }
    
    
    
    let a = replaceSize - inputSize
    guard a > 885 else { print("Wrong answer"); return }
    print("Part 2: \(a)")
}


