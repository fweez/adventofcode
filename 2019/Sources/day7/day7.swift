import AOCShared
import Foundation
import Overture

/// To start a new day, copy and paste this file into a new dayX directory in Sources/
public func part1() { print("Part 1: \(findLargestThrusterSignal(parseFile(opcodeParser, "day7.txt").first!))") }
public func part2() { print("Part 2: \(findLargestThrusterSignalWithFeedback(parseFile(opcodeParser, "day7.txt").first!))") }

func tgt(_ a: ([Int], Int), _ b: ([Int], Int)) -> ([Int], Int) {
    if b.1 > a.1 { return b }
    else { return a }
}

func findLargestThrusterSignal(_ memory: [Int]) -> Int {
    iterateAmplifierInput(
        phaseSettings: Array(0...4),
        program: ProgramState(
            memory: memory,
            input: 0))
        .reduce(([Int](), Int.min), tgt)
        .1
}

/// Given a list of amplifier phase settings, iterate over them, running the given function to generate the input to the next amp; returning the list of inputs and their outputs
func iterateAmplifierInput(phaseSettings: [Int], amplifierInput: Int = 0, program: ProgramState) -> [([Int], Int)] {
    phaseSettings
        .enumerated()
        .flatMap { idx, phaseSetting -> [([Int], Int)] in
            var program = program
            program.inputs = [phaseSetting, amplifierInput]
            return runIntcodeProgram(program)
                .map { finishedProgram -> [([Int], Int)] in
                    var nextPhaseSettings = phaseSettings
                    _ = nextPhaseSettings.remove(at: idx)
                    let nextAmpInput = finishedProgram.output
                    if nextPhaseSettings.isEmpty {
                        return [([phaseSetting], nextAmpInput)]
                    } else {
                        return iterateAmplifierInput(phaseSettings: nextPhaseSettings, amplifierInput: nextAmpInput, program: program)
                            .map { output in
                                ([phaseSetting] + output.0, output.1)
                            }
                    }
                } ?? [([Int.min], Int.min)]
        }
}

func findLargestThrusterSignalWithFeedback(_ memory: [Int]) -> Int {
    allIterations(Array((5...9)))
        .map { $0.map { ProgramState(memory: memory, input: $0) } }
        .map(processAmpsWithFeedback)
        .reduce(Int.min, max)
}

func allIterations(_ a: [Int]) -> [[Int]] {
    a
        .enumerated()
        .flatMap { idx, curr -> [[Int]] in
            var nextArray = a
            _ = nextArray.remove(at: idx)
            if nextArray.isEmpty { return [[curr]] }
            else { return allIterations(nextArray).map { [curr] + $0 } }
        }
}

func processAmpsWithFeedback(amplifiers: [ProgramState]) -> Int {
    let initialInput = amplifiers.last!.output
    let newAmps = amplifiers
        .reduce([ProgramState]()) { amps, curr in
            let inputSignal = amps.last?.output ?? initialInput
            let newAmp = runIntcodeProgram(setInput(inputSignal, curr))
            return amps + [newAmp!]
        }
    if case .stopped = newAmps.first?.runState { return newAmps.last?.output ?? Int.min }
    return processAmpsWithFeedback(amplifiers: newAmps)
}
