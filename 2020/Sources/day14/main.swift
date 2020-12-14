import AOCShared
import Foundation
import Overture

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

run(input: input, parse, part1, part2)

enum MaskBit {
    case none
    case one
    case zero
}

enum Command {
    case mask([MaskBit])
    case memset(addr: UInt, value: UInt)
}

typealias ParsedStructure = [Command]

func parse(_ input: String) -> ParsedStructure {
    let mask = zeroOrMore(oneOf([literal("1", MaskBit.one), literal("0", MaskBit.zero), literal("X", MaskBit.none)]))
    let maskCommand = zip(literal("mask = "), mask)
        .map { _, m in Command.mask(m) }
    let memCommand = zip(
        literal("mem["),
        intParser,
        literal("] = "),
        intParser)
        .map { _, addr, _, val in Command.memset(addr: UInt(addr), value: UInt(val)) }
    let command = oneOf([maskCommand, memCommand])
    return input
        .split(separator: "\n")
        .compactMap(pipe(String.init, command.runStatic))
}

struct Memory {
    let mask: [MaskBit]
    let memory: [UInt: UInt]
}

let zeroMask = Array(repeating: MaskBit.none, count: 36)

func debugMask(_ mask: [MaskBit]) {
    print(String(mask.map { b -> Character in
        switch b {
        case .none: return "X"
        case .one: return "1"
        case .zero: return "0"
        }
    }))
}

func masked(value: UInt, mask: [MaskBit]) -> UInt {
    mask
        .reversed()
        .enumerated()
        .reduce(value) { value, t in
            switch t.element  {
            case .none: return value
            case .one: return value | (1 << t.offset)
            case .zero: return value & (UInt.max - (1 << t.offset))
            }
        }
}

func part1(_ parsedInput: ParsedStructure) {
    let value = parsedInput
        .reduce(Memory(mask: zeroMask, memory: [:])) { memory, command in
            switch command {
            case .mask(let mask): return Memory(mask: mask, memory: memory.memory)
            case .memset(addr: let addr, value: let rawValue):
                var d = memory.memory
                d[addr] = masked(value: rawValue, mask: memory.mask)
                return Memory(mask: memory.mask, memory: d)
            }
        }
        .memory
        .values
        .reduce(0, +)
    print("Part 1: \(value)")
}

func decode(addr: UInt, mask: [MaskBit], index: Int) -> [UInt] {
    guard index < mask.count else { return [addr] }
    switch mask[mask.count - index - 1] {
    case .zero: return decode(addr: addr, mask: mask, index: index + 1)
    case .one:  return decode(addr: addr | (1 << index), mask: mask, index: index + 1)
    case .none:
        let zeroBit = ~(~addr | (1 << index))
        let oneBit = addr | (1 << index)
        return decode(addr: zeroBit, mask: mask, index: index + 1) + decode(addr: oneBit, mask: mask, index: index + 1)
    }
}

func part2(_ parsedInput: ParsedStructure) {
    let value = parsedInput
        .reduce(Memory(mask: zeroMask, memory: [:])) { memory, command in
            switch command {
            case .mask(let mask): return Memory(mask: mask, memory: memory.memory)
            case .memset(addr: let addr, value: let value):
                let d = Set(decode(addr: addr, mask: memory.mask, index: 0))
                    .reduce(into: memory.memory) { memory, addr in
                        memory[addr] = value
                    }
                
                return Memory(mask: memory.mask, memory: d)
            }
        }
        .memory
        .values
        .reduce(0, +)
    print("Part 2: \(value)")
}
