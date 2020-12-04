import AOCShared
import Foundation
import Overture

public typealias ParsedStructure = [[String: String]]

func validHeight(_ s: String) -> Bool {
    guard s.count > 2, let v = Int(s.prefix(s.count - 2)) else { return false }
    switch s.suffix(2) {
    case "cm": return v >= 150 && v <= 193
    case "in": return v >= 59 && v <= 76
    default: return false
    }
}

func validHair(_ s: String) -> Bool {
    s.count == 7 &&
        s.prefix(1) == "#" &&
        s.suffix(6).allSatisfy("0123456789abcdef".contains)
}

func validEye(_ s: String) -> Bool {
    ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].contains(s)
}

func validPid(_ s: String) -> Bool {
    s.count == 9 &&
        s.allSatisfy { $0.isNumber }
}
    
func valid(checking d: [String: String]) -> Bool {
    guard let b = d["byr"], let ib = Int(b), ib >= 1920, ib <= 2002 else { return false }
    guard let i = d["iyr"], let ii = Int(i), ii >= 2010, ii <= 2020 else { return false }
    guard let e = d["eyr"], let ie = Int(e), ie >= 2020, ie <= 2030 else { return false }
    guard let h = d["hgt"], validHeight(h) else { return false }
    guard let c = d["hcl"], validHair(c) else { return false }
    guard let ec = d["ecl"], validEye(ec) else { return false }
    guard let p = d["pid"], validPid(p) else { return false }
    return true
}

func valid(_ d: [String: String]) -> Bool {
    ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"].allSatisfy(d.keys.contains)
}

let kvParser = zip(
    alphaParser,
    literal(":"),
    nonWhitespaceParser
).map { a, _, b in (String(a), String(b)) }

let passportDictParser: Parser<[(String, String)], String> = zeroOrMore(kvParser, separatedBy: oneOf([literal(" "), literal("\n")]))
let fileParser = zeroOrMore(passportDictParser, separatedBy: literal("\n\n"))

public func parse(_ input: String) -> ParsedStructure {
    (try? String(contentsOfFile: input)).map { s in
        fileParser
            .runStatic(s)?
            .compactMap(Dictionary.init)
            ?? []
    } ?? []
}

func uncheckedPassports(_ dicts: [[String: String]]) -> Int {
    dicts.reduce(0) { accum, d in
        if valid(d) { return accum + 1 }
        else { return accum }
    }
}

func checkedPassports(_ dicts: [[String: String]]) -> Int {
    dicts.reduce(0) { accum, d in
        if valid(checking: d) { return accum + 1 }
        else { return accum }
    }
}

public func part1(_ parsedInput: ParsedStructure) {
    print("Part 1: \(uncheckedPassports(parsedInput))")
}
public func part2(_ parsedInput: ParsedStructure) {
    print("Part 2: \(checkedPassports(parsedInput)) ") }
