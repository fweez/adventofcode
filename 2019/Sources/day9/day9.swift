import AOCShared
import Foundation
import Overture

/// To start a new day, copy and paste this file into a new dayX directory in Sources/
public func parse(_ filename: String) -> ProgramState {
    ProgramState(memory: parseFile(opcodeParser, filename).first!)
    
}
public func part1(_ p: ProgramState) { print("Part 1: \(getBoostKeyCode(p))") }
public func part2(_ p: ProgramState) { print("Part 2: \(getDistressSignal(p))") }

func getBoostKeyCode(_ p: ProgramState) -> Int {
    var p = p
    p.inputs = [1]
    let finished = runIntcodeProgram(p)
    return finished?.output ?? Int.min
}

func getDistressSignal(_ p: ProgramState) -> Int {
    var p = p
    p.inputs = [2]
    let finished = runIntcodeProgram(p)
    return finished?.output ?? Int.min
}
