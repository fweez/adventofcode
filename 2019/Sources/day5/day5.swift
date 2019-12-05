import AOCShared
import Foundation
import Overture

/// To start a new day, copy and paste this file into a new dayX directory in Sources/
public func part1() { print("Part 1: \(getDiagnosticCode())") }
public func part2() { print("Part 2: \(testThermalRadiator())") }

func getDiagnosticCode() -> Int {
    let program = parseFile(opcodeParser, "day5.txt").first!
    return runIntcodeProgram(ProgramState(memory: program, input: 1))?.output ?? Int.max
}

func testThermalRadiator() -> Int {
    let program = parseFile(opcodeParser, "day5.txt").first!
    return runIntcodeProgram(ProgramState(memory: program, input: 5))?.output ?? Int.max
}
