import Overture

enum Opcode: Int {
    case add = 1
    case mul = 2
    case end = 99
}

typealias ProgramState = (memory: [Int], pointer: Int)

public func runIntcodeProgram(_ memory: [Int], _ pointer: Int = 0) -> [Int]? {
    guard let op = Opcode.init(rawValue: memory[pointer]) else { return nil }
    var runOp: ((ProgramState) -> ProgramState?)? = nil
    switch op {
    case .add: runOp = operation(+)
    case .mul: runOp = operation(*)
    case .end: return memory
    }
    return runOp?((memory, pointer))
        .flatMap(runIntcodeProgram)
}

func operation(_ f: @escaping (Int, Int) -> Int) -> (ProgramState) -> ProgramState? {
    return { state in
        let (memory, pointer) = state
        guard (pointer + 4) < memory.count else { return nil }
        let a = memory[pointer + 1]
        let b = memory[pointer + 2]
        let dest = memory[pointer + 3]
        guard a < memory.count, b < memory.count, dest < memory.count else { return nil }
        var newMemoryState = memory
        newMemoryState[dest] = f(memory[a], memory[b])
        return (newMemoryState, pointer + 4)
    }
}
