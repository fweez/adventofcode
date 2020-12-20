import AOCShared
import Foundation
import Overture

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

testP1Inputs()
testP2Inputs()

run(input: input, parse, part1, part2)

indirect enum AST: Equatable {
    case value(Int)
    case plus(AST, AST)
    case mult(AST, AST)
}

enum Token {
    case lparen
    case rparen
    case value(Int)
    case plus
    case mult
}

typealias ParsedStructure = [[Token]]

func parse(_ input: String) -> ParsedStructure {
    input
        .split(separator: "\n")
        .map(tokenize)
}

func tokenize(_ input: Substring) -> [Token] {
    zeroOrMore(oneOf([
        literal("(").map { Token.lparen },
        literal(")").map { Token.rparen },
        intParser.map(Token.value),
        literal(" + ").map { Token.plus },
        literal(" * ").map { Token.mult }
    ]))
    .runStatic(String(input)) ?? []
}

func toP1AST(_ tokens: inout [Token]) -> AST {
    var stack: [AST] = []
    while true {
        guard tokens.count > 0 else {
            guard stack.count == 1 else { fatalError() }
            return stack.first!
        }
        switch tokens.removeFirst() {
        case .lparen:
            stack.append(toP1AST(&tokens))
        case .rparen:
            guard stack.count == 1 else { fatalError() }
            return stack.first!
        case .value(let i):
            stack.append(.value(i))
        case .plus:
            stack.append(p1Oper(AST.plus, prev: stack.removeLast(), tokens: &tokens))
        case .mult:
            stack.append(p1Oper(AST.mult, prev: stack.removeLast(), tokens: &tokens))
        }
    }
}

func p1Oper(_ ast: (AST, AST) -> AST, prev: AST, tokens: inout [Token]) -> AST {
    let next = tokens.removeFirst()
    switch next {
    case .value(let i):
        return ast(prev, .value(i))
    case .lparen:
        return ast(prev, toP1AST(&tokens))
    default:
        fatalError("Unexpected next")
    }
}

func p1execute(_ ast: AST) -> Int {
    switch ast {
    case .value(let i): return i
    case let .plus(a, b): return p1execute(a) + p1execute(b)
    case let .mult(a, b): return p1execute(a) * p1execute(b)
    }
}

func p2execute(_ tokens: inout [Token]) -> Int {
    // scan tokens. for each:
    var stack: [Token] = []
    while tokens.count > 0 {
        var done = false
        let next = tokens.removeFirst()
        switch next {
        // if it's a value, put it on the stack
        case .value: stack.append(next)
        
        case .plus:
            // if it's a plus, add the last item on the stack to
            let last = stack.removeLast()
            guard case .value(let i) = last else { fatalError() }
            let next = tokens.removeFirst()
            switch next {
            //   if next token is a value, the value
            case .value(let j): stack.append(.value(i + j))
            //   if next token is a lparen, evaluate the subexpression into a value and use that
            case .lparen: stack.append(.value(i + p2execute(&tokens)))
            default: fatalError()
            }
        // defer calculations of mults to next pass
        case .mult: stack.append(next)
        // if it's a rparen, exit the loop and evaluate the stack
        case .rparen: done = true
        case .lparen: stack.append(.value(p2execute(&tokens)))
        }
        if done { break }
    }
    
    return p1execute(toP1AST(&stack))
}

func testP1Inputs() {
    func parseToAST(_ s: String) -> [AST] {
        parse(s)
            .map { tokens in
                var tokens = tokens
                return toP1AST(&tokens)
            }
    }
    assert(parseToAST("5") == [AST.value(5)])
    assert(p1execute(parseToAST("5").first!) == 5)
    assert(parseToAST("5 + 1") == [AST.plus(.value(5), .value(1))])
    assert(p1execute(parseToAST("5 + 1").first!) == 6)
    assert(parseToAST("5 * 2") == [AST.mult(.value(5), .value(2))])
    assert(p1execute(parseToAST("5 * 2").first!) == 10)
    assert(parseToAST("(5 + 1)") == [AST.plus(.value(5), .value(1))])
    assert(p1execute(parseToAST("(5 + 1)").first!) == 6)
    assert(parseToAST("5 + (2 + 1)") == [
                    AST.plus(
                        .value(5),
                        AST.plus(
                            .value(2),
                            .value(1)
                        ))])
    assert(p1execute(parseToAST("5 + (2 + 1)").first!) == 8)
    assert(parseToAST("5 + 2 * 3").first! == AST.mult(.plus(.value(5), .value(2)), .value(3)))
    assert(p1execute(parseToAST("5 + 2 * 3").first!) == 21)
    assert(p1execute(parseToAST("2 * 3 + (4 * 5)").first!) == 26)
    assert(p1execute(parseToAST("5 + (8 * 3 + 9 + 3 * 4 * 3)").first!) == 437)
    assert(p1execute(parseToAST("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))").first!) == 12240)
    assert(parseToAST("5 + (1 + 2) * 3").first! == .mult(.plus(.value(5), .plus(.value(1), .value(2))), .value(3)))
    assert(p1execute(parseToAST("5 + (1 + 2) * 3").first!) == 24)
    assert(p1execute(parseToAST("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2").first!) == 13632)
}

func testP2Inputs() {
    func exec(_ s: String) -> Int {
        var tokens = tokenize(s[...])
        return p2execute(&tokens)
    }
    assert(exec("1 + (2 * 3) + (4 * (5 + 6))") == 51)
    assert(exec("2 * 3 + (4 * 5)") == 46)
    assert(exec("5 + (8 * 3 + 9 + 3 * 4 * 3)") == 1445)
    assert(exec("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") == 669060)
    assert(exec("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") == 23340)
}

func part1(_ parsedInput: ParsedStructure) {
    let sum = parsedInput
        .map { tokens in
            var tokens = tokens
            return toP1AST(&tokens)
        }
        .reduce(0) { accum, line in accum + p1execute(line) }
    print("Part 1: \(sum)")
}



func part2(_ parsedInput: ParsedStructure) {
    let sum = parsedInput.reduce(0) { accum, tokens in
        var tokens = tokens
        return accum + p2execute(&tokens)
    }
    print("Part 2: \(sum)")
}

