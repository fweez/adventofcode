//
//  main.swift
//  day16
//
//  Created by Ryan Forsythe on 12/16/18.
//  Copyright © 2018 Zero Gravitas. All rights reserved.
//

import Foundation

struct Instruction {
    let opcode: Int
    let a: Int
    let b: Int
    let c: Int
    
    init(_ opcode: Int = ILLEGAL_REGISTER_VALUE, _ a: Int = ILLEGAL_REGISTER_VALUE, _ b: Int = ILLEGAL_REGISTER_VALUE, _ c: Int = ILLEGAL_REGISTER_VALUE) {
        self.opcode = opcode
        self.a = a
        self.b = b
        self.c = c
    }
    
    var valid: Bool {
        return self.opcode != ILLEGAL_REGISTER_VALUE && self.a != ILLEGAL_REGISTER_VALUE && self.b != ILLEGAL_REGISTER_VALUE && self.c != ILLEGAL_REGISTER_VALUE
    }
}

extension Instruction: CustomStringConvertible {
    var description: String { return "\(self.opcode) \(self.a) \(self.b) \(self.c)" }
}

extension Instruction: Hashable {
    func hash(into hasher: inout Hasher) {
        hasher.combine(self.opcode)
        hasher.combine(self.a)
        hasher.combine(self.b)
        hasher.combine(self.c)
    }
}

enum Opcode: CaseIterable {
    case addr,
    addi,
    mulr,
    muli,
    banr,
    bani,
    borr,
    bori,
    setr,
    seti,
    gtir,
    gtri,
    gtrr,
    eqir,
    eqri,
    eqrr
    
    func run(before: [Int], instruction: Instruction) -> [Int] {
        var out = Array(before)
        switch self {
        // addr (add register) stores into register C the result of adding register A and register B.
        case .addr: out[instruction.c] = before[instruction.a] + before[instruction.b]
        // addi (add immediate) stores into register C the result of adding register A and value B.
        case .addi: out[instruction.c] = before[instruction.a] + instruction.b
        // mulr (multiply register) stores into register C the result of multiplying register A and register B.
        case .mulr: out[instruction.c] = before[instruction.a] * before[instruction.b]
        // muli (multiply immediate) stores into register C the result of multiplying register A and value B.
        case .muli: out[instruction.c] = before[instruction.a] * instruction.b
        // banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
        case .banr: out[instruction.c] = before[instruction.a] & before[instruction.b]
        // bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
        case .bani: out[instruction.c] = before[instruction.a] & instruction.b
        // borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
        case .borr: out[instruction.c] = before[instruction.a] | before[instruction.b]
        // bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
        case .bori: out[instruction.c] = before[instruction.a] | instruction.b
        // setr (set register) copies the contents of register A into register C. (Input B is ignored.)
        case .setr: out[instruction.c] = before[instruction.a]
        // seti (set immediate) stores value A into register C. (Input B is ignored.)
        case .seti: out[instruction.c] = instruction.a
        // gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
        case .gtir: out[instruction.c] = instruction.a > before[instruction.b] ? 1 : 0
        // gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
        case .gtri: out[instruction.c] = before[instruction.a] > instruction.b ? 1 : 0
        // gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
        case .gtrr: out[instruction.c] = before[instruction.a] > before[instruction.b] ? 1 : 0
        // eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
        case .eqir: out[instruction.c] = instruction.a == before[instruction.b] ? 1 : 0
        // eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
        case .eqri: out[instruction.c] = before[instruction.a] == instruction.b ? 1 : 0
        // eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
        case .eqrr: out[instruction.c] = before[instruction.a] == before[instruction.b] ? 1 : 0
        }
        return out
    }
}

enum ParseError: Error {
    case BadInput
}

let ILLEGAL_REGISTER_VALUE = -1

class Compy {
    // Map (instructions, before register array) to lists of opcodes they match
    //var opcodeMatches: [(Instruction, [Int]): [Opcode]] = [:]

    convenience init?(inputFile: String) throws {
        self.init()
        if let filePath = Bundle.main.path(forResource: inputFile, ofType: "txt"),
            let input = try? String(contentsOfFile: filePath) {
            try self.load(input)
        } else if let input = try? String(contentsOfFile: inputFile) {
            try self.load(input)
        } else {
            print("Couldn't load file")
            return nil
        }
    }

    func load(_ input: String) throws -> Int {
        var before = Array(repeating: ILLEGAL_REGISTER_VALUE, count: 4)
        var instruction = Instruction()
        //self.opcodeMatches = [:]
        
        /*
         Before: [0, 0, 3, 0]
         11 0 1 0
         After:  [1, 0, 3, 0]

         */
        func convertToInts(_ registerBlock: Substring) -> [Int] {
            return registerBlock.split(separator: ",")
                .map { Int($0.filter({ $0 != " "})) ?? ILLEGAL_REGISTER_VALUE }
        }
        
        enum LineType {
            case BeforeLine, InstructionLine, AfterLine//, NewLine
        }
        var currentLine = LineType.BeforeLine
        var blockCount = 0
        var manyMatchCount = 0
        for line in input.split(separator: "\n") {
            switch currentLine {
            case .BeforeLine:
                if line.prefix(9) != "Before: [" { break }
                print("Before line: \(line)")
                let registerBlock = line[line.index(line.startIndex, offsetBy: 9)..<line.index(line.endIndex, offsetBy: -1)]
                before = convertToInts(registerBlock)
                currentLine = .InstructionLine
            case .InstructionLine:
                print("Instruction line: \(line)")
                let instructionComponents = line.split(separator: " ").map { Int($0) ?? ILLEGAL_REGISTER_VALUE }
                instruction = Instruction(instructionComponents[0], instructionComponents[1], instructionComponents[2], instructionComponents[3])
                currentLine = .AfterLine
            case .AfterLine:
                print("After line: \(line)")
                let registerBlock = line[line.index(line.startIndex, offsetBy: 9)..<line.index(line.endIndex, offsetBy: -1)]
                let after = convertToInts(registerBlock)
                assert(instruction.valid)
                let matchingOpcodes = self.findMatchingOpcodes(registers: before, instruction: instruction, result: after)
                if matchingOpcodes.count >= 3 {
                    manyMatchCount += 1
                }
                instruction = Instruction()
                before = Array(repeating: ILLEGAL_REGISTER_VALUE, count: 4)
                currentLine = .BeforeLine
                blockCount += 1
            }
        }
        
        print("Processed \(blockCount) items")
        //let count = opcodeMatches.values.map({ $0.count }).filter({ $0 >= 3 }).count
        print("Part A: \(manyMatchCount) (of \(blockCount)) have >= 3 matches")
        return manyMatchCount
    }
    
    func findMatchingOpcodes(registers: [Int], instruction: Instruction, result: [Int]) -> [Opcode] {
        return Opcode.allCases.filter { (opcode) -> Bool in
            opcode.run(before: registers, instruction: instruction) == result
        }
    }
}

if CommandLine.arguments.count > 0 && CommandLine.arguments.last?.suffix(3) == "txt" {
    guard let m = try Compy(inputFile: CommandLine.arguments.last!) else {
        print("Couldn't load input file")
        exit(1)
    }
} else {
    print("Running tests")
    let compy = Compy()
    // from problem description
    let out = compy.findMatchingOpcodes(registers: [3, 2, 1, 1], instruction: Instruction(9, 2, 1, 2), result: [3, 2, 2, 1])
    assert(out.contains(.mulr))
    assert(out.contains(.addi))
    assert(out.contains(.seti))
    assert(out.count == 3)
    
    assert(Opcode.addr.run(before: [1, 3, 9, 1], instruction: Instruction(1, 0, 1, 2)) == [1, 3, 4, 1])
    assert(Opcode.addi.run(before: [1, 3, 9, 1], instruction: Instruction(1, 0, 1, 2)) == [1, 3, 2, 1])
    assert(Opcode.mulr.run(before: [1, 3, 9, 1], instruction: Instruction(1, 0, 1, 2)) == [1, 3, 3, 1])
    assert(Opcode.muli.run(before: [1, 3, 9, 1], instruction: Instruction(1, 0, 1, 2)) == [1, 3, 1, 1])
    assert(Opcode.banr.run(before: [1, 2, 9, 1], instruction: Instruction(1, 0, 1, 2)) == [1, 2, 0, 1])
    assert(Opcode.bani.run(before: [1, 2, 9, 1], instruction: Instruction(1, 0, 1, 2)) == [1, 2, 1, 1])
    assert(Opcode.borr.run(before: [1, 2, 9, 1], instruction: Instruction(1, 0, 1, 2)) == [1, 2, 3, 1])
    assert(Opcode.bani.run(before: [1, 2, 9, 1], instruction: Instruction(1, 0, 4, 2)) == [1, 2, 0, 1])
    assert(Opcode.setr.run(before: [1, 2, 9, 1], instruction: Instruction(1, 0, 7, 2)) == [1, 2, 1, 1])
    assert(Opcode.seti.run(before: [1, 2, 9, 1], instruction: Instruction(1, 0, 7, 2)) == [1, 2, 0, 1])
    assert(Opcode.gtir.run(before: [1, 2, 9, 1], instruction: Instruction(1, 0, 1, 2)) == [1, 2, 0, 1])
    assert(Opcode.gtri.run(before: [1, 2, 9, 1], instruction: Instruction(1, 0, 1, 2)) == [1, 2, 0, 1])
    assert(Opcode.gtrr.run(before: [1, 2, 9, 1], instruction: Instruction(1, 0, 1, 2)) == [1, 2, 0, 1])
    
    let count = try compy.load("""
    Before: [3, 2, 1, 1]
    11 2 1 2
    After:  [3, 2, 2, 1]

    Before: [3, 2, 1, 1]
    10 2 1 2
    After:  [3, 2, 2, 1]

    Before: [3, 2, 1, 1]
    9 2 1 2
    After:  [3, 2, 2, 1]
    """)
    assert(count == 3)
}
