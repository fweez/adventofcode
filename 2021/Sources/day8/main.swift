import AOCShared
import Foundation
import Overture
import Parsing

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

struct Entry {
    let signals: [String]
    let output: [String]
}

typealias ParsedStructure = [Entry]

let signals = Many(CharacterSet.lowercaseLetters, atLeast: 10, atMost: 10, separator: " ")
let output = Many(CharacterSet.lowercaseLetters, atLeast: 4, atMost: 4, separator: " ")
let entry = signals
    .skip(" | ")
    .take(output)
    .map { Entry(signals: $0.map(String.init), output: $1.map(String.init)) }
let parser: AnyParser<Substring, ParsedStructure> = Many(entry, separator: "\n").eraseToAnyParser()

run(input: input, parser, part1, part2)
// or
//public func parse(_ input: String) -> ParsedStructure { 1 }
//run(input: input, parse, part1, part2)

func p1(_ parsedInput: ParsedStructure) -> Int {
    parsedInput
        .map { $0
            .output
            .filter {
                $0.count == 2 ||
                $0.count == 3 ||
                $0.count == 4 ||
                $0.count == 7
            }
            .count
        }
        .reduce(0, +)
}

func part1(_ parsedInput: ParsedStructure) {
    let a = p1(parsedInput)
    print("Part 1: \(a)")
}

func value(for entry: Entry) -> Int {
    typealias DigitPredicate = (String) -> Bool
    let one: DigitPredicate = { $0.count == 2 }
    let four: DigitPredicate = { $0.count == 4 }
    let seven: DigitPredicate = { $0.count == 3 }
    let eight: DigitPredicate = { $0.count == 7 }

    let oneInput: () -> String = { entry.signals.first(where: one) ?? "" }
    let fourInput: () -> String = { entry.signals.first(where: four) ?? "" }
    let fnoInput: () -> String = {
        let oi = oneInput()
        return fourInput().filter { oi.contains($0) == false }
    }
    let fourNotOneCountOne: DigitPredicate = { s in s.filter(fnoInput().contains).count == 1 }
    let fourNotOneCountTwo: DigitPredicate = { s in s.filter(fnoInput().contains).count == 2 }
    let oneCountOne: DigitPredicate = { s in s.filter(oneInput().contains).count == 1 }
    let oneCountTwo: DigitPredicate = { s in s.filter(oneInput().contains).count == 2 }
    let two: DigitPredicate = { s in
            s.count == 5 &&
            oneCountOne(s) &&
            fourNotOneCountOne(s)
        }
    let three: DigitPredicate = { s in s.count == 5 && oneCountTwo(s) }
    let five: DigitPredicate = { s in s.count == 5 && fourNotOneCountTwo(s) }
    let six: DigitPredicate = { s in s.count == 6 && oneCountOne(s) }
    let zero: DigitPredicate = { s in s.count == 6 && !six(s) && fourNotOneCountOne(s) }
    let nine: DigitPredicate = { s in s.count == 6 && !six(s) && !zero(s) }
    
    let decoder: [DigitPredicate] = [zero, one, two, three, four, five, six, seven, eight, nine]
    return entry.output
        .map { s in decoder.firstIndex { f in f(s) }! }
        .reversed()
        .enumerated()
        .reduce(0) { (accum: Int, t: (offset: Int, element: Int)) -> Int in
            accum + (Int(pow(10.0, Double(t.offset))) * (t.element))
        }
}

func outputValue(_ parsedInput: ParsedStructure) -> Int {
    parsedInput
        .map(value)
        .reduce(0, +)
}

func part2(_ parsedInput: ParsedStructure) {
    let a = outputValue(parsedInput)
    guard a < 1170367 else { print("Wrong answer \(a)"); return }
    print("Part 2: \(a)") }

func test2_1() {
    let input = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
    guard let parsed = parser.parse(input)?.first else { fatalError("bad parse") }
    assert(value(for: parsed) == 5353)
}

test2_1()

func test2_2() {
    let input = """
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
"""
    guard let parsed = parser.parse(input) else { fatalError("bad parse") }
    let expected = [8394, 9781, 1197, 9361, 4873, 8418, 4548, 1625, 8717, 4315]
    zip(parsed, expected)
        .forEach { entry, expect in
            let v = value(for: entry)
            assert(v == expect, "expected \(expect), got \(v)")
            print(v)
        }
    assert(outputValue(parsed) == 61229)
}

test2_2()
