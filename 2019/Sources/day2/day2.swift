import AOCShared
import Foundation
import Overture

/// To start a new day, copy and paste this file into a new dayX directory in Sources/
public func part1() { print("Part 1: \(restore())") }
public func part2() { print("Part 2: \(findInputs())") }

let opcodeParser: Parser<[Int], String> = zeroOrMore(
    optionalPrefix(while: { $0.isNumber }),
    separatedBy: literal(","))
    .map { $0.compactMap(pipe(String.init, Int.init)) }

func restore() -> Int {
    let file = ingestFile("day2.txt")
    guard var line = file.first else { preconditionFailure("Didn't read file") }
    guard var matches = opcodeParser.run(&line) else { preconditionFailure("Didn't parse") }
    matches[1] = 12
    matches[2] = 2
    return runIntcodeProgram(matches)?.first ?? Int.max
}

func findInputs() -> Int {
    let file = ingestFile("day2.txt")
    guard var line = file.first else { preconditionFailure("Didn't read file") }
    guard let originalMemory = opcodeParser.run(&line) else { preconditionFailure("didn't parse") }
    for noun in Array((1...1000)) {
        for verb in Array((1...1000)) {
            var mem = originalMemory
            mem[1] = noun
            mem[2] = verb
            if runIntcodeProgram(mem)?.first == 19690720 { return (noun * 100) + verb }
        }
    }
    return Int.max
}
