import AOCShared
import Foundation
import Overture
import Parsing
import Collections

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

typealias Board = [[Int]]
typealias ParsedStructure = (numbers: OrderedSet<Int>, boards: [Board])

let bingoNumbers = Many(Int.parser(), separator: ",")
    .map { OrderedSet($0) }
let boardRow: AnyParser<Substring, [Int]> = Prefix { $0 == " " }
    .take(Many(
        Int.parser(),
        atLeast: 5,
        atMost: 5,
        separator: Prefix { $0 == " "}))
    .map { _, r in r }
    .eraseToAnyParser()
let board: AnyParser<Substring, [[Int]]> = Many(
    boardRow,
    atLeast: 5,
    atMost: 5,
    separator: "\n")
    .eraseToAnyParser()
let boards = Many(board, separator:"\n\n")
let parser: AnyParser<Substring, ParsedStructure> = bingoNumbers
    .skip("\n\n")
    .take(boards)
    .map { (numbers: $0.0, boards: $0.1) }
    .eraseToAnyParser()

run(input: input, parser, part1, part2)

func nilMin(_ a: Int?, _ b: Int?) -> Int? {
    if let a = a,
       let b = b {
        return min(b, a)
    }
    if let a = a {
        return a
    }
    if let b = b {
        return b
    }
    return nil
}

func winIdx(_ numbers: OrderedSet<Int>) -> ([Int]) -> Int? {
    { row in
        let idxs = row.compactMap(numbers.firstIndex(of:))
        guard idxs.count == row.count else { return nil }
        return idxs.max()
    }
}

func winIdx(_ numbers: OrderedSet<Int>) -> (Board) -> Int? {
    { board in
        nilMin(
            board
                .compactMap(winIdx(numbers))
                .min(),
            (0..<5)
                .map { col in board.map { $0[col] } }
                .compactMap(winIdx(numbers))
                .min()
        )
    }
}

func boardSearch(_ parsedInput: ParsedStructure, scoreCmp: (Int, Int) -> Bool) -> (Int, Int) {
    let (numbers, boards) = parsedInput
    let winFinder: (Board) -> Int? = winIdx(numbers)
    let t = boards
        .enumerated()
        .compactMap { idx, board -> (Int, Int)? in winFinder(board).map { (idx, $0) } }
        .min { a, b in scoreCmp(a.1, b.1) }
    
    guard let t = t else { fatalError("You fucked up") }
    
    let (boardIdx, winIdx) = t
    let calledNumbers = numbers.prefix(through: winIdx)
    let boardNums = boards[boardIdx]
        .map { row in row.filter { calledNumbers.contains($0) == false }}
        .joined()
    let boardSum = boardNums
        .reduce(0, +)
    let winNum = numbers[winIdx]
    
    return (boardSum, winNum)
}

func part1(_ parsedInput: ParsedStructure) {
    let (sum, winNum) = boardSearch(parsedInput, scoreCmp: <)
    let answer = sum * winNum
    guard answer < 4711 else { fatalError("\(answer) already failed") }
    print("Part 1: \(answer)")
}

func part2(_ parsedInput: ParsedStructure) {
    let (sum, winNum) = boardSearch(parsedInput, scoreCmp: >)
    let answer = sum * winNum
    print("Part 1: \(answer)")
}

func testp1() {
    let testInput = """
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7

"""
    guard let testParsed = parser.parse(testInput) else { fatalError() }
    let (sum, winNum) = boardSearch(testParsed, scoreCmp: <)
    assert(sum == 188)
    assert(winNum == 24)
}

//testp1()
