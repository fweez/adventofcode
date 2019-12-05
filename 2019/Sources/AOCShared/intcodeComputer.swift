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
    
    init?(rawValue: Int) {
//        print("Opcode \(rawValue)")
        let opcode = rawValue.remainderReportingOverflow(dividingBy: 100).partialValue
        switch opcode {
        case 1:
            guard let o: Opcode = Opcode.get2ParameterModes(rawValue).map({ .add($0, $1) }) else { return nil }
            self = o
        case 2:
            guard let o: Opcode = Opcode.get2ParameterModes(rawValue).map({ .mul($0, $1) }) else { return nil }
            self = o
        case 3:
            self = .put
        case 4:
            guard let o: Opcode = ParameterMode.init(rawValue: (rawValue / 100).remainderReportingOverflow(dividingBy: 10).partialValue)
                .map({ .get($0) }) else { return nil }
            self = o
        case 5:
            guard let o: Opcode = Opcode.get2ParameterModes(rawValue).map({ .jumpIfTrue($0, $1) }) else { return nil }
            self = o
        case 6:
            guard let o: Opcode = Opcode.get2ParameterModes(rawValue).map({ .jumpIfFalse($0, $1) }) else { return nil }
            self = o
        case 7:
            guard let o: Opcode = Opcode.get2ParameterModes(rawValue).map({ .lessThan($0, $1) }) else { return nil }
            self = o
        case 8:
            guard let o: Opcode = Opcode.get2ParameterModes(rawValue).map({ .equals($0, $1) }) else { return nil }
            self = o
        case 99:
            self = .end
        default:
            preconditionFailure("Unknown opcode \(opcode) (from \(rawValue))")
            return nil
        }
    }
    
    
    func run(_ state: ProgramState) -> ProgramState? {
        switch self {
        case let .add(m1, m2): return operation(+, m1, m2)(state)
        case let .mul(m1, m2): return operation(*, m1, m2)(state)
        case .put: return putOpt(state)
        case .get(let m1): return getOpt(m1)(state)
        case let .jumpIfTrue(m1, m2): return jumpIfTrueOpt(m1, m2)(state)
        case let .jumpIfFalse(m1, m2): return jumpIfFalseOpt(m1, m2)(state)
        case let .lessThan(m1, m2): return lessThanOpt(m1, m2)(state)
        case let .equals(m1, m2): return equalsOpt(m1, m2)(state)
        case .end: return endOpt(state)
        
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

func operation(_ f: @escaping (Int, Int) -> Int, _ m1: ParameterMode, _ m2: ParameterMode) -> (ProgramState) -> ProgramState? {
    return { state in
        fetch3(state, m1, m2)
            .flatMap { a, b, dest, state in
                guard dest < state.memory.count else { return nil }
                var newState = state
                newState.memory[dest] = f(a, b)
                return newState
            }
    }
}

func putOpt(_ state: ProgramState) -> ProgramState? {
    guard state.pointer < state.memory.count else { return nil }
    let dest = state.memory[state.pointer]
    var newState = state
    newState.memory[dest] = state.input
    // eat input?
    newState.pointer += 1
    return newState
}

func getOpt(_ mode: ParameterMode) -> (ProgramState) -> ProgramState? {
    return { state in
        get(state.pointer, mode, state)
            .map { dest in
                var newState = state
                newState.output = newState.memory[dest]
                newState.pointer += 1
                return newState
        }
    }
}

func jumpIfTrueOpt(_ m1: ParameterMode, _ m2: ParameterMode) -> (ProgramState) -> ProgramState? {
    return { state in
        fetch2(state, m1)
            .flatMap { a, pb, state in
                guard a != 0 else { return state }
                return get(pb, m2, state)
                    .map { with(state, set(\.pointer, $0)) }
        }
    }
}

func jumpIfFalseOpt(_ m1: ParameterMode, _ m2: ParameterMode) -> (ProgramState) -> ProgramState? {
    return { state in
        fetch2(state, m1)
            .flatMap { a, pb, state in
                guard a == 0 else { return state }
                return get(pb, m2, state)
                    .map { with(state, set(\.pointer, $0)) }
            }
    }
}

func lessThanOpt(_ m1: ParameterMode, _ m2: ParameterMode) -> (ProgramState) -> ProgramState? {
    return { state in
        fetch3(state, m1, m2)
            .flatMap { a, b, dest, state in
                var state = state
                if a < b { state.memory[dest] = 1 }
                else { state.memory[dest] = 0 }
                return state
            }
    }
}

func equalsOpt(_ m1: ParameterMode, _ m2: ParameterMode) -> (ProgramState) -> ProgramState? {
    return { state in
        fetch3(state, m1, m2)
            .flatMap { a, b, dest, state in
                var state = state
                if a == b { state.memory[dest] = 1 }
                else { state.memory[dest] = 0 }
                return state
        }
    }
}

func endOpt(_ state: ProgramState) -> ProgramState? {
    var state = state
    state.running = false
    return state
}

func fetchOp(_ state: ProgramState) -> (Opcode, ProgramState)? {
    guard let op = Opcode.init(rawValue: state.memory[state.pointer]) else { return nil }
    var state = state
    state.pointer += 1
    return (op, state)
}

func fetch2(_ state: ProgramState, _ m1: ParameterMode) -> (Int, Int, ProgramState)? {
    guard (state.pointer + 2) < state.memory.count else { return nil }
    let pa = state.memory[state.pointer]
    guard let a = get(pa, m1, state) else { return nil }
    let dest = state.memory[state.pointer + 1]
    var state = state
    state.pointer += 2
    return (a, dest, state)
}

func fetch3(_ state: ProgramState, _ m1: ParameterMode, _ m2: ParameterMode) -> (Int, Int, Int, ProgramState)? {
    fetch2(state, m1)
        .flatMap { a, pb, state in
            guard let b = get(pb, m2, state) else { return nil }
            guard state.pointer  < state.memory.count else { return nil }
            let dest = state.memory[state.pointer]
            var state = state
            state.pointer += 1
            return (a, b, dest, state)
        }
}

public func runIntcodeProgram(_ memory: [Int], _ pointer: Int = 0) -> [Int]? {
    runIntcodeProgram(ProgramState(memory: memory, pointer: pointer, input: Int.max, output: Int.max))?.memory
}

public func runIntcodeProgram(_ state: ProgramState) -> ProgramState? {
//    print("State: pointer \(state.pointer), input: \(state.input), output: \(state.output)")
//    dump(state.memory)
    return fetchOp(state)
        .flatMap { op, state -> ProgramState? in
//            dump(op)
            return op.run(state) }
        .flatMap { state -> ProgramState? in
//            dump(state.memory)
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
