import AOCShared
import Foundation
import Overture
import Parsing

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

indirect enum Instruction {
    case and(Instruction, Instruction)
    case or(Instruction, Instruction)
    case not(Instruction)
    case lsh(Instruction, UInt16)
    case rsh(Instruction, UInt16)
    case int(UInt16)
    case key(String)
    case assign(Instruction)
}


let int = UInt16.parser()
    .map(Instruction.int)
    .eraseToAnyParser()
let key = CharacterSet.lowercaseLetters
    .map { Instruction.key(String($0)) }
    .eraseToAnyParser()
let ident = OneOfMany(int, key).eraseToAnyParser()
let and = ident
    .skip(" AND ")
    .take(ident)
    .map(Instruction.and)
    .eraseToAnyParser()
let or = ident
    .skip(" OR ")
    .take(ident)
    .map(Instruction.or)
    .eraseToAnyParser()
let not = "NOT "
    .take(ident)
    .map { _, s in Instruction.not(s) }
    .eraseToAnyParser()
let lsh = ident
    .skip(" LSHIFT ")
    .take(UInt16.parser())
    .map(Instruction.lsh)
    .eraseToAnyParser()
let rsh = ident
    .skip(" RSHIFT ")
    .take(UInt16.parser())
    .map(Instruction.rsh)
    .eraseToAnyParser()
let assign = ident
    .map(Instruction.assign)
    .eraseToAnyParser()
let inst = OneOfMany(and, or, not, lsh, rsh, assign)
let line: AnyParser<Substring, (String, Instruction)> = inst
    .skip(" -> ")
    .take(CharacterSet.lowercaseLetters)
    .map { (String($1), $0) }
    .eraseToAnyParser()

typealias ParsedStructure = [String: Instruction]

let parser: AnyParser<Substring, ParsedStructure> = Many(line, separator: "\n")
    .map { lines in Dictionary(uniqueKeysWithValues: lines) }
    .eraseToAnyParser()
run(input: input, parser, part1, part2)
// or
//func parse(_ input: String) -> ParsedStructure { 1 }
//run(input: input, parse, part1, part2)

func getValue(_ key: String, network: inout ParsedStructure) -> UInt16 {
    guard let i = network[key] else { fatalError() }
    let v: UInt16 = {
        switch i {
        case .int(let v): return v
        case .assign(let i):
            return getValue(i, network: &network)
        case let .and(a, b):
            return getValue(a, network: &network) & getValue(b, network: &network)
        case let .or(a, b):
            return getValue(a, network: &network) | getValue(b, network: &network)
        case let .not(a):
            return ~getValue(a, network: &network)
        case let .lsh(a, v):
            return getValue(a, network: &network) << v
        case let .rsh(a, v):
            return getValue(a, network: &network) >> v
        case .key(let s):
            return getValue(s, network: &network)
        }
    }()
    network[key] = .int(v)
    return v
}

func getValue(_ instruction: Instruction, network: inout ParsedStructure) -> UInt16 {
    switch instruction {
    case .int(let v): return v
    case .key(let s): return getValue(s, network: &network)
    default: fatalError()
    }
}

func part1(_ parsedInput: ParsedStructure) {
    var parsedInput = parsedInput
    let a = getValue(.key("a"), network: &parsedInput)
    print("Part 1: \(a)")
}

func part2(_ parsedInput: ParsedStructure) {
    var p1 = parsedInput
    let a = getValue(.key("a"), network: &p1)
    var p2 = parsedInput
    p2["b"] = .int(a)
    let a2 = getValue(.key("a"), network: &p2)
    print("Part 2: \(a2)")
}
