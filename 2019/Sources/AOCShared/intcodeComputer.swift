import Overture

var debug = false
func debugprint(_ message: @autoclosure () -> String) {
    guard debug else { return }
    print(message())
}

enum ParameterMode: Int {
    case position = 0
    case immediate = 1
    case relative = 2
}

public enum ProgramRunState {
    case running
    case awaitingInput
    case stopped
}

public struct ProgramState {
    public var memory: [Int: Int]
    public var pointer: Int
    public var inputs: [Int] = []
    public var inputBlock: (() -> Int)? = nil
    public var outputs: [Int] = []
    public var outputBlock: ((Int) -> Void)? = nil
    public var runState: ProgramRunState = .running
    public var relativeBase = 0
    
    public var output: Int { outputs.last ?? Int.min }
    
    public init(memory: [Int], pointer: Int = 0, input: Int? = nil) {
        self.memory = Dictionary<Int, Int>(uniqueKeysWithValues: Array(memory.enumerated()))
        self.pointer = pointer
        if let input = input { self.inputs = [input] }
        debugprint("Initialized ProgramState with \(self.memory.count) item program; fp at \(self.pointer); input is \(self.inputs)")
    }
    
    public init(memory: [Int], inputBlock: @escaping () -> Int, outputBlock: @escaping (Int) -> Void) {
        self.init(memory: memory)
        self.inputBlock = inputBlock
        self.outputBlock = outputBlock
    }
}

enum Opcode {
    case add(ParameterMode, ParameterMode, ParameterMode)
    case mul(ParameterMode, ParameterMode, ParameterMode)
    case put(ParameterMode)
    case get(ParameterMode)
    case jumpIfTrue(ParameterMode, ParameterMode)
    case jumpIfFalse(ParameterMode, ParameterMode)
    case lessThan(ParameterMode, ParameterMode, ParameterMode)
    case equals(ParameterMode, ParameterMode, ParameterMode)
    case adjustRelativeBase(ParameterMode)
    case end
    
    init(rawValue: Int) {
        debugprint("Opcode \(rawValue)")
        let opcode = rawValue.remainderReportingOverflow(dividingBy: 100).partialValue
        let m1 = ParameterMode.init(rawValue: (rawValue / 100).remainderReportingOverflow(dividingBy: 10).partialValue)!
        let m2 = ParameterMode.init(rawValue: (rawValue / 1000).remainderReportingOverflow(dividingBy: 10).partialValue)!
        let m3 = ParameterMode.init(rawValue: (rawValue / 10000).remainderReportingOverflow(dividingBy: 10).partialValue)!
        switch opcode {
        case 1: self = .add(m1, m2, m3)
        case 2: self = .mul(m1, m2, m3)
        case 3: self = .put(m1)
        case 4: self = .get(m1)
        case 5: self = .jumpIfTrue(m1, m2)
        case 6: self = .jumpIfFalse(m1, m2)
        case 7: self = .lessThan(m1, m2, m3)
        case 8: self = .equals(m1, m2, m3)
        case 9: self = .adjustRelativeBase(m1)
        case 99: self = .end
        default:
            preconditionFailure("Unknown opcode \(opcode) (from \(rawValue))")
        }
    }
    
    var run: (ProgramState) -> ProgramState? {
        debugprint("Run \(self)")
        switch self {
        case let .add(m1, m2, m3): return operation(+, m1, m2, m3)
        case let .mul(m1, m2, m3): return operation(*, m1, m2, m3)
        case .put(let m1): return putOpt(m1)
        case .get(let m1): return getOpt(m1)
        case let .jumpIfTrue(m1, m2): return jumpIfTrueOpt(m1, m2)
        case let .jumpIfFalse(m1, m2): return jumpIfFalseOpt(m1, m2)
        case let .lessThan(m1, m2, m3): return lessThanOpt(m1, m2, m3)
        case let .equals(m1, m2, m3): return equalsOpt(m1, m2, m3)
        case .adjustRelativeBase(let m1): return adjustRelativeOpt(m1)
        case .end: return endOpt
        }
    }
}

func get(_ position: Int, _ mode: ParameterMode, _ state: ProgramState) -> Int? {
    debugprint("Get \(position) (mode \(mode))")
    switch mode {
    case .position:
        debugprint("--> \(state.memory[position] == nil ? "<new memory> 0" : "\(state.memory[position]!)")")
        return state.memory[position] ?? 0
    case .immediate:
        debugprint("--> \(position)")
        return position
    case .relative:
        debugprint("resolved relative addr: \(position + state.relativeBase)")
        debugprint("--> \(state.memory[position + state.relativeBase] ?? Int.min)")
        return state.memory[position + state.relativeBase] ?? 0
    }
}

func getInput(_ state: ProgramState) -> (Int?, ProgramState) {
    if let b = state.inputBlock {
        debugprint("Getting input from an input block")
        return (b(), state)
    }
    debugprint("Get input from \(state.inputs)")
    var state = state
    guard state.inputs.count > 0 else {
        state.runState = .awaitingInput
        state.pointer -= 1 // rewind the fp back so we re-process this getInput op next time through.
        return (nil, state)
    }
    let next = state.inputs.removeFirst()
    return (next, state)
}

func setMemory(_ value: Int, _ position: Int, _ state: ProgramState) -> ProgramState? {
    debugprint("Set \(position) to \(value)")
    var newState = state
    newState.memory[position] = value
    return newState
}

func setOutput(_ value: Int, _ state: ProgramState) -> ProgramState {
    if let b = state.outputBlock {
        debugprint("Setting output to output block")
        b(value)
    }
    debugprint("Set output to \(value)")
    var state = state
    state.outputs.append(value)
    return state
}

public func setInput(_ value: Int, _ state: ProgramState) -> ProgramState {
    debugprint("Set input to \(value)")
    var state = state
    if case .awaitingInput = state.runState { state.runState = .running }
    state.inputs.append(value)
    return state
}

func operation(_ f: @escaping (Int, Int) -> Int, _ m1: ParameterMode, _ m2: ParameterMode, _ m3: ParameterMode) -> (ProgramState) -> ProgramState? {
    { state in
        fetch3(state, m1, m2, destMode: m3)
            .map { (f($0, $1), $2, $3) }
            .flatMap(setMemory)
    }
}

func putOpt(_ mode: ParameterMode) -> (ProgramState) -> ProgramState? {
    { state in
        let (input, state) = getInput(state)
        if state.runState == .awaitingInput { return state }
        return input.flatMap { input in
            fetch(state, .immediate)
                .flatMap { dest, state -> ProgramState? in
                    switch mode {
                    case .position:
                        return setMemory(input, dest, state)
                    case .immediate:
                        fatalError("Cannot put in immediate mode")
                    case .relative:
                        return setMemory(input, dest + state.relativeBase, state)
                    }
                }
        }
    }
}

func getOpt(_ mode: ParameterMode) -> (ProgramState) -> ProgramState? {
    { state in
        fetch(state, mode)
            .flatMap(setOutput)
    }
}

func jumpIfTrueOpt(_ m1: ParameterMode, _ m2: ParameterMode) -> (ProgramState) -> ProgramState? {
    { state in
        fetch2(state, m1, m2)
            .flatMap { a, b, state in
                guard a != 0 else { return state }
                return with(state, set(\.pointer, b))
            }
    }
}

func jumpIfFalseOpt(_ m1: ParameterMode, _ m2: ParameterMode) -> (ProgramState) -> ProgramState? {
    { state in
        fetch2(state, m1, m2)
            .flatMap { a, b, state in
                guard a == 0 else { return state }
                return with(state, set(\.pointer, b))
            }
    }
}

func lessThanOpt(_ m1: ParameterMode, _ m2: ParameterMode, _ m3: ParameterMode) -> (ProgramState) -> ProgramState? {
    { state in
        fetch3(state, m1, m2, destMode: m3)
            .flatMap { a, b, dest, state in
                if a < b { return setMemory(1, dest, state) }
                else { return setMemory(0, dest, state) }
            }
    }
}

func equalsOpt(_ m1: ParameterMode, _ m2: ParameterMode, _ m3: ParameterMode) -> (ProgramState) -> ProgramState? {
    { state in
        fetch3(state, m1, m2, destMode: m3)
            .flatMap { a, b, dest, state in
                if a == b { return setMemory(1, dest, state) }
                else { return setMemory(0, dest, state) }
        }
    }
}

func adjustRelativeOpt(_ m1: ParameterMode) -> (ProgramState) -> ProgramState? {
    { state in
        fetch(state, m1)
            .flatMap { a, state in
                over(\ProgramState.relativeBase, { $0 + a })(state)
            }
    }
}

let endOpt = set(\ProgramState.runState, .stopped)

func fetch(_ state: ProgramState, _ m1: ParameterMode) -> (Int, ProgramState)? {
    debugprint("Fetch memory[\(state.pointer)]")
    return state.memory[state.pointer]
        .flatMap { get($0, m1, state) }
        .flatMap { ($0, over(\ProgramState.pointer, { $0 + 1 })(state)) }
}

func fetch2(_ state: ProgramState, _ m1: ParameterMode, _ m2: ParameterMode) -> (Int, Int, ProgramState)? {
    fetch(state, m1)
        .flatMap { a, state in
            fetch(state, m2)
                .map { (a, $0, $1) }
        }
}

func fetch3(_ state: ProgramState, _ m1: ParameterMode, _ m2: ParameterMode, destMode: ParameterMode) -> (Int, Int, Int, ProgramState)? {
    fetch2(state, m1, m2)
        .flatMap { a, b, state in
            fetch(state, .immediate)
                .map { dp, state in
                    switch destMode {
                    case .immediate: fatalError("Destination cannot be in immediate mode")
                    case .position: return (a, b, dp, state)
                    case .relative: return (a, b, state.relativeBase + dp, state)
                    }
                }
        }
}

public func runIntcodeProgram(_ memory: [Int], _ pointer: Int = 0) -> [Int]? {
    runIntcodeProgram(ProgramState(memory: memory, pointer: pointer, input: Int.max))?.memory
        .sorted { $0.0 < $1.0 }
        .map { $0.1 }
}

public func runIntcodeProgram(_ state: ProgramState) -> ProgramState? {
//    debugprint("State: pointer \(state.pointer), input: \(state.input), output: \(state.output)")
//    dump(state.memory)
    var state: ProgramState? = state
    while let oldState = state, oldState.runState == .running {
        let newState = fetch(oldState, .immediate)
            .flatMap { a, state -> ProgramState? in
                debugprint("Fetched opcode, function pointer at \(state.pointer), relative base at \(state.relativeBase)")
                return Opcode(rawValue: a).run(state)
        }
        state = newState
        if let state = state {
            debugprint("Ran opcode, function pointer at \(state.pointer), relative base at \(state.relativeBase)")
            debugprint("Program state: \(state.runState)")
        }
    }
    return state
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
