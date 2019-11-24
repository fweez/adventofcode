//
//  main.swift
//  day19
//
//  Created by Ryan Forsythe on 12/19/18.
//  Copyright © 2018 Zero Gravitas. All rights reserved.
//

import Foundation

struct Instruction {
    let opcode: Opcode
    let a: Int
    let b: Int
    let c: Int
    
    init(_ opcode: Opcode, _ a: Int = ILLEGAL_REGISTER_VALUE, _ b: Int = ILLEGAL_REGISTER_VALUE, _ c: Int = ILLEGAL_REGISTER_VALUE) {
        self.opcode = opcode
        self.a = a
        self.b = b
        self.c = c
    }
    
    var valid: Bool {
        return self.a != ILLEGAL_REGISTER_VALUE && self.b != ILLEGAL_REGISTER_VALUE && self.c != ILLEGAL_REGISTER_VALUE
    }
    
    func run(_ registers: [Int]) -> [Int] {
        return self.opcode.run(before: registers, instruction: self)
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

enum Opcode: String, CaseIterable {
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
    
//    init?(fromString: String) {
//        switch fromString {
//        case "addr": self = .addr
//        case "addi": self = .addi
//        case "mulr": self = .mulr
//        case "muli": self = .muli
//        case "banr": self = .banr
//        case "bani": self = .bani
//        case "borr": self = .borr
//        case "bori": self = .bori
//        case "setr": self = .setr
//        case "seti": self = .seti
//        case "gtir": self = .gtir
//        case "gtri": self = .gtri
//        case "gtrr": self = .gtrr
//        case "eqir": self = .eqir
//        case "eqri": self = .eqri
//        case "eqrr": self = .eqrr
//        default: return nil
//        }
//    }
//
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

class CompyIIE {
    let registerCount = 6
    
    convenience init?(inputFile: String) throws {
        self.init()
        if let filePath = Bundle.main.path(forResource: inputFile, ofType: "txt"),
            let input = try? String(contentsOfFile: filePath) {
            _ = try self.load(input)
        } else if let input = try? String(contentsOfFile: inputFile) {
            _ = try self.load(input)
        } else {
            print("Couldn't load file")
            return nil
        }
    }
    
    func load(_ input: String, initialRegisters: [Int]? = nil) throws -> [Int] {
        var before = Array(repeating: ILLEGAL_REGISTER_VALUE, count: self.registerCount)
        func convertToInts(_ registerBlock: Substring) -> [Int] {
            return registerBlock.split(separator: ",")
                .map { Int($0.filter({ $0 != " "})) ?? ILLEGAL_REGISTER_VALUE }
        }
        
        var program: [Instruction] = []
        var ipRegister = 0
        
        for line in input.split(separator: "\n") {
            //#ip 1
            if line.prefix(3) == "#ip" {
                // Note, there's only one of these in the program
                let split = line.split(separator: " ")
                guard split.count == 2 else { throw ParseError.BadInput }
                ipRegister = Int(split.last!) ?? ILLEGAL_REGISTER_VALUE
                continue
            }
            
            //addi 1 16 1
            let split = line.split(separator: " ")
            guard let opcode = Opcode(rawValue: String(split.first!)) else { throw ParseError.BadInput }
            let instructionComponents = split[1...].map { Int($0) ?? ILLEGAL_REGISTER_VALUE }
            guard instructionComponents.count == 3 else { throw ParseError.BadInput }
            let instruction = Instruction(opcode, instructionComponents[0], instructionComponents[1], instructionComponents[2])
            program.append(instruction)
        }
        
        // Run program
        var registers: [Int]
        if let r = initialRegisters {
            registers = r
        } else {
            registers = Array(repeating: 0, count: self.registerCount)
        }
        while true {
            if registers[ipRegister] >= program.count || registers[ipRegister] < 0 { return registers }
            let before = registers
            registers = program[registers[ipRegister]].run(registers)
            print("ip: \(registers[ipRegister])\t\(before)\t\(program[registers[ipRegister]])\t\(registers)")
            registers[ipRegister] += 1
        }
    }
}

let compy = CompyIIE()
var programText = """
#ip 0
seti 5 0 1
seti 6 0 2
addi 0 1 0
addr 1 2 3
setr 1 0 0
seti 8 0 4
seti 9 0 5
"""
if let out = try? compy.load(programText) {
    print("Test: \(out)")
}
programText = """
#ip 1
addi 1 16 1
seti 1 1 3
seti 1 9 5
mulr 3 5 2
eqrr 2 4 2
addr 2 1 1
addi 1 1 1
addr 3 0 0
addi 5 1 5
gtrr 5 4 2
addr 1 2 1
seti 2 6 1
addi 3 1 3
gtrr 3 4 2
addr 2 1 1
seti 1 6 1
mulr 1 1 1
addi 4 2 4
mulr 4 4 4
mulr 1 4 4
muli 4 11 4
addi 2 6 2
mulr 2 1 2
addi 2 2 2
addr 4 2 4
addr 1 0 1
seti 0 3 1
setr 1 4 2
mulr 2 1 2
addr 1 2 2
mulr 1 2 2
muli 2 14 2
mulr 2 1 2
addr 4 2 4
seti 0 0 0
seti 0 4 1
"""
//if let out = try? compy.load(programText) {
//    print("Part A: \(out)")
//}

if let out = try? compy.load(programText, initialRegisters: [8, 7, 0, 5, 10551370, 10551369]) {
    print("Part B: \(out)")
}

