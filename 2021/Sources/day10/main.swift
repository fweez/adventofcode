import AOCShared
import Foundation
import Overture
//import Parsing

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

public typealias ParsedStructure = [Substring]

public func parse(_ input: String) -> ParsedStructure { input.split(separator: "\n") }

run(input: input, parse, part1, part2)

enum ParseError: Error {
    case unexpected(Character)
}

func takeAndExpect(_ c: Character, _ input: inout String) -> Result<String, ParseError> {
    _ = input.removeFirst()
    let r = parenParser(&input)
    switch r {
    case .failure: return r
    case .success(let completion):
        guard let next = input.first else {
            return .success(completion + [c])
        }
        if next == c {
            _ = input.removeFirst()
        } else {
            return .failure(.unexpected(next))
        }
    }
    
    return parenParser(&input)
}

func parenParser(_ input: inout String) -> Result<String, ParseError> {
    guard let peek = input.first else { return .success("") }
    switch peek {
    case "(": return takeAndExpect(")", &input)
    case "[": return takeAndExpect("]", &input)
    case "{": return takeAndExpect("}", &input)
    case "<": return takeAndExpect(">", &input)
    default: return .success("")
    }
}

func scoreP1(_ cs: [Character]) -> Int {
    cs.reduce(0) { accum, c in
        switch c {
        case ")": return accum + 3
        case "]": return accum + 57
        case "}": return accum + 1197
        case ">": return accum + 25137
        default: fatalError()
        }
    }
}

func p2Map(_ c: Character) -> Int {
    switch c {
    case ")": return 1
    case "]": return 2
    case "}": return 3
    case ">": return 4
    default: fatalError()
    }
}

func scoresP2(_ completions: [String]) -> [Int] {
    completions
        .map { s in
            s.reduce(0) { accum, c in
                (accum * 5) + p2Map(c)
            }
        }
        .sorted()
}

func scoreP2(_ completions: [String]) -> Int {
    scoresP2(completions)[completions.count / 2]
}

public func part1(_ parsedInput: ParsedStructure) {
    let bads = parsedInput.compactMap { line -> Character? in
        var line = String(line)
        switch parenParser(&line) {
        case .failure(.unexpected(let c)): return c
        case .success: return nil
        }
    }
    let a = scoreP1(bads)
    
    print("Part 1: \(a)")
}

public func part2(_ parsedInput: ParsedStructure) {
    let completed = parsedInput.compactMap { line -> String? in
        var completedLine = String(line)
        switch parenParser(&completedLine) {
        case .failure: return nil
        case .success(let completion): return completion
        }
    }
    let score = scoreP2(completed)
    print("Part 2: \(score)")
}

func testp1() {
    let input = """
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
""".split(separator: "\n")
    let expectation: [Character?] = [nil, nil, "}", nil, ")", "]", nil, nil, ">"]
    zip(input, expectation)
        .forEach { i, e in
            guard e != nil else { return }
            var s = String(i)
            switch parenParser(&s) {
            case .success: return
            case .failure(.unexpected(let bad)):
                assert(bad == e, "while parsing \(i): Expected \(e ?? "X"), got \(bad ?? "X"), unparsed \(s)")
            }
        }
}

testp1()

func testp2() {
    let input = """
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
(((({<>}<{<{<>}{[]{[]{}
{<[[]]>}<{[{[{[]{()[[[]
<{([{{}}[<[[[<>{}]]]>[]]
""".split(separator: "\n")
    let expectation = """
}}]])})]
)}>]})
}}>}>))))
]]}}]}]}>
])}>
""".split(separator: "\n")
    let c = zip(input, expectation)
        .map { i, e -> String in
            var s = String(i)
            switch parenParser(&s) {
            case .success(let completion):
                assert(completion == e, "while parsing \(i): Expected \(e), got \(completion), unparsed \(s)")
                return completion
            case .failure:
                fatalError()
            }
        }
    assert(scoreP2(c) == 288957)
}

testp2()
