import AOCShared
import Foundation
import Overture

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

run(input: input, parse, part1, part2)

enum Bus {
    case id(Int)
    case invalid
}

struct Schedule {
    let departure: Int
    let buses: [Bus]
}

typealias ParsedStructure = Schedule

func parse(_ input: String) -> ParsedStructure {
    let invalidBus = literal("x", Bus.invalid)
    let validBus = intParser.map(Bus.id)
    
    return zip(
        intParser,
        literal("\n"),
        zeroOrMore(oneOf([validBus, invalidBus]), separatedBy: literal(",")))
        .map { departure, _, buses in
            Schedule(departure: departure, buses: buses)
        }
        .runStatic(input)
        ?? Schedule(departure: 0, buses: [])
}

func waitTime(_ departure: Int, _ busId: Int) -> Int {
    guard busId > 0 else { return Int.max }
    let r = departure % busId
    return busId - r
}

func part1(_ parsedInput: ParsedStructure) {
    let t = parsedInput.buses
        .reduce((-1, Int.max)) { curr, busId in
            switch busId {
            case .id(let busId):
                let t = waitTime(parsedInput.departure, busId)
                if t < curr.1 {
                    return (busId, t)
                } else {
                    return curr
                }
            case .invalid: return curr
            }
        }
    print("Part 1: \(t.0 * t.1)")
}

func commonMultiple(adding: Int, buses: [Bus]) -> Int {
    buses.reduce(1) { accum, bus in
        switch bus {
        case .invalid: return accum
        case .id(let i): return (i + 1) * accum
        }
    }
}

func test(departureTime: Int, inc: Int, buses: [Bus]) -> Int? {
    nil
}

func part2(_ parsedInput: ParsedStructure) {
    let v = test(departureTime: 1 << 32, inc: 1 << 32, buses: parsedInput.buses) ?? Int.min
    print("Part 2: \(v)") }
