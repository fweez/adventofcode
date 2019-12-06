import Overture

enum ParameterMode: Int {
    case position = 0
    case immediate = 1
}

public struct ProgramState {
    public var memory: [Int]
    public var pointer: Int
    public var input: Int
    public var output: Int
    public var running: Bool = true
    
    public init(memory: [Int], pointer: Int = 0, input: Int, output: Int = Int.max) {
        self.memory = memory
        self.pointer = pointer
        self.input = input
        self.output = output
        
    }
}

enum Opcode {
    case add(ParameterMode, ParameterMode)
    case mul(ParameterMode, ParameterMode)
    case put
    case get(ParameterMode)
    case jumpIfTrue(ParameterMode, ParameterMode)
    case jumpIfFalse(ParameterMode, ParameterMode)
    case lessThan(ParameterMode, ParameterMode)
    case equals(ParameterMode, ParameterMode)
    case end
    
    static func get2ParameterModes(_ rawValue: Int) -> (ParameterMode, ParameterMode)? {
        zip(get1ParameterMode(rawValue),
            ParameterMode.init(rawValue: (rawValue / 1000).remainderReportingOverflow(dividingBy: 10).partialValue))
    }
    
    static func get1ParameterMode(_ rawValue: Int) -> ParameterMode? {
        ParameterMode.init(rawValue: (rawValue / 100).remainderReportingOverflow(dividingBy: 10).partialValue)
    }
    
    init(rawValue: Int) {
//        print("Opcode \(rawValue)")
        let opcode = rawValue.remainderReportingOverflow(dividingBy: 100).partialValue
        let m1 = ParameterMode.init(rawValue: (rawValue / 100).remainderReportingOverflow(dividingBy: 10).partialValue)!
        let m2 = ParameterMode.init(rawValue: (rawValue / 1000).remainderReportingOverflow(dividingBy: 10).partialValue)!
        switch opcode {
        case 1: self = .add(m1, m2)
        case 2: self = .mul(m1, m2)
        case 3: self = .put
        case 4: self = .get(m1)
        case 5: self = .jumpIfTrue(m1, m2)
        case 6: self = .jumpIfFalse(m1, m2)
        case 7: self = .lessThan(m1, m2)
        case 8: self = .equals(m1, m2)
        case 99: self = .end
        default:
            preconditionFailure("Unknown opcode \(opcode) (from \(rawValue))")
        }
    }
    
    var run: (ProgramState) -> ProgramState? {
        switch self {
        case let .add(m1, m2): return operation(+, m1, m2)
        case let .mul(m1, m2): return operation(*, m1, m2)
        case .put: return putOpt
        case .get(let m1): return getOpt(m1)
        case let .jumpIfTrue(m1, m2): return jumpIfTrueOpt(m1, m2)
        case let .jumpIfFalse(m1, m2): return jumpIfFalseOpt(m1, m2)
        case let .lessThan(m1, m2): return lessThanOpt(m1, m2)
        case let .equals(m1, m2): return equalsOpt(m1, m2)
        case .end: return endOpt
        }
    }
}

func get(_ position: Int, _ mode: ParameterMode, _ state: ProgramState) -> Int? {
    switch mode {
    case .position:
        guard position < state.memory.count else { return nil }
        return state.memory[position]
    case .immediate:
        return position
    }
}

func set(_ value: Int, _ position: Int, _ state: ProgramState) -> ProgramState? {
    guard position < state.memory.count else { return nil }
    var newState = state
    newState.memory[position] = value
    return newState
}

func operation(_ f: @escaping (Int, Int) -> Int, _ m1: ParameterMode, _ m2: ParameterMode) -> (ProgramState) -> ProgramState? {
    return { state in
        fetch3(state, m1, m2)
            .map { (f($0, $1), $2, $3) }
            .flatMap(set)
    }
}

func putOpt(_ state: ProgramState) -> ProgramState? {
    fetch(state)
        .flatMap { set($1.input, $0, $1) }
}

func getOpt(_ mode: ParameterMode) -> (ProgramState) -> ProgramState? {
    return { state in
        fetch(state)
            .flatMap { pa, state in
                get(pa, mode, state)
                    .map { ($0, state) }
            }
            .map { value, state in
                var state = state
                state.output = value
                return state
            }
    }
}

func jumpIfTrueOpt(_ m1: ParameterMode, _ m2: ParameterMode) -> (ProgramState) -> ProgramState? {
    { state in
        fetch2(state, m1)
            .flatMap { a, pb, state in
                guard a != 0 else { return state }
                return get(pb, m2, state)
                    .map { with(state, set(\.pointer, $0)) }
        }
    }
}

func jumpIfFalseOpt(_ m1: ParameterMode, _ m2: ParameterMode) -> (ProgramState) -> ProgramState? {
    { state in
        fetch2(state, m1)
            .flatMap { a, pb, state in
                guard a == 0 else { return state }
                return get(pb, m2, state)
                    .map { with(state, set(\.pointer, $0)) }
            }
    }
}

func lessThanOpt(_ m1: ParameterMode, _ m2: ParameterMode) -> (ProgramState) -> ProgramState? {
    { state in
        fetch3(state, m1, m2)
            .flatMap { a, b, dest, state in
                if a < b { return set(1, dest, state) }
                else { return set(0, dest, state) }
            }
    }
}

func equalsOpt(_ m1: ParameterMode, _ m2: ParameterMode) -> (ProgramState) -> ProgramState? {
    { state in
        fetch3(state, m1, m2)
            .flatMap { a, b, dest, state in
                if a == b { return set(1, dest, state) }
                else { return set(0, dest, state) }
        }
    }
}

func endOpt(_ state: ProgramState) -> ProgramState? {
    var state = state
    state.running = false
    return state
}

func fetch(_ state: ProgramState) -> (Int, ProgramState)? {
    guard state.pointer < state.memory.count else { return nil }
    let a = state.memory[state.pointer]
    var state = state
    state.pointer += 1
    return (a, state)
}

func fetch2(_ state: ProgramState, _ m1: ParameterMode) -> (Int, Int, ProgramState)? {
    fetch(state)
        .flatMap { pa, state in
            get(pa, m1, state)
                .map { ($0, state) }
        }
        .flatMap { a, state in
            fetch(state)
                .map { (a, $0, $1) }
        }
}

func fetch3(_ state: ProgramState, _ m1: ParameterMode, _ m2: ParameterMode) -> (Int, Int, Int, ProgramState)? {
    fetch2(state, m1)
        .flatMap { a, pb, state in
            get(pb, m2, state)
                .map { (a, $0, state) }
        }
        .flatMap { a, b, state in
            fetch(state)
                .map { (a, b, $0, $1) }
        }
}

public func runIntcodeProgram(_ memory: [Int], _ pointer: Int = 0) -> [Int]? {
    runIntcodeProgram(ProgramState(memory: memory, pointer: pointer, input: Int.max, output: Int.max))?.memory
}

public func runIntcodeProgram(_ state: ProgramState) -> ProgramState? {
//    print("State: pointer \(state.pointer), input: \(state.input), output: \(state.output)")
//    dump(state.memory)
    return fetch(state)
        .flatMap { pa, state in
            Opcode(rawValue: pa).run(state)
        }
        .flatMap { state -> ProgramState? in
            guard state.running else { return state }
            return runIntcodeProgram(state)
        }
}

let intParser = zip(
    zeroOrMore(literal("-")),
    optionalPrefix(while: { $0.isNumber }))
    .map { sgn, val -> Int in
        if sgn.count > 0 { return Int(val)! * -1 }
        else { return Int(val)! }
}

public let opcodeParser: Parser<[Int], String> = zeroOrMore(
    intParser,
    separatedBy: literal(","))
