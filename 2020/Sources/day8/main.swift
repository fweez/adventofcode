import AOCShared
import Foundation
import Overture

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

run(input: input, parse, part1, part2)

enum Instruction: CaseIterable {
    case acc(Int)
    case jmp(Int)
    case nop(Int)
    
    static var allCases: [Instruction] {
        [.acc(Int.min), .jmp(Int.min), .nop(Int.min)]
    }
    
    static let sign = oneOf([literal("+", 1), literal("-", -1)])
    static let instructionInt = zip(sign, intParser).map { $0 * $1 }
    
    static func instructionParserMaker(_ key: String, f: @escaping (Int) -> Instruction) -> Parser<Instruction, String> {
        zip(literal("\(key) "), instructionInt).map { _, i in f(i) }
    }
    
    var parser: Parser<Instruction, String> {
        switch self {
        case .acc: return Self.instructionParserMaker("acc", f: Self.acc)
        case .jmp: return Self.instructionParserMaker("jmp", f: Self.jmp)
        case .nop: return Self.instructionParserMaker("nop", f: Self.nop)
        }
    }
}

struct Gameboy {
    let accumulator: Int
    let ip: Int
    
    func run(_ i: Instruction) -> Gameboy {
        switch i {
        case .acc(let i): return Gameboy(accumulator: accumulator + i, ip: ip + 1)
        case .jmp(let i): return Gameboy(accumulator: accumulator, ip: ip + i)
        case .nop: return Gameboy(accumulator: accumulator, ip: ip + 1)
        }
    }
}

typealias ParsedStructure = [Instruction]

func parse(_ input: String) -> ParsedStructure {
    let instructionParser = oneOf(Instruction.allCases.map { $0.parser })
    return input
        .split(separator: "\n")
        .compactMap { instructionParser.runStatic(String($0)) }
}

var originalSeenIndices: Set<Int> = []
func part1(_ parsedInput: ParsedStructure) {
    let (gb, s) = runToLoopOrCompletion(instructions: parsedInput)
    originalSeenIndices = s
    print("Part 1: \(gb.accumulator)")
}

func runToLoopOrCompletion(instructions: [Instruction]) -> (Gameboy, Set<Int>) {
    var gb = Gameboy(accumulator: 0, ip: 0)
    var seenIndices = Set<Int>()
    while true {
        seenIndices.insert(gb.ip)
        if gb.ip == instructions.count { return (gb, seenIndices) }
        guard gb.ip < instructions.count else { fatalError("ip \(gb.ip) went past end of instruction list \(instructions.count)")}
        gb = gb.run(instructions[gb.ip])
        guard seenIndices.contains(gb.ip) == false else { return (gb, seenIndices) }
    }
}

func part2(_ instructions: ParsedStructure) {
    let gb = originalSeenIndices
        .map { index -> Gameboy in
            var instructions = instructions
            switch instructions[index] {
            case .acc: return Gameboy(accumulator: 0, ip: 0)
            case .nop(let i): instructions[index] = .jmp(i)
            case .jmp(let i): instructions[index] = .nop(i)
            }
            return runToLoopOrCompletion(instructions: instructions).0
        }
        .first { $0.ip == instructions.count }
        
    print("Part 2: \(gb?.accumulator ?? Int.min)")
}

