import AOCShared
import Foundation
import Overture

run(input: "8,13,1,0,18,9", parse, part1, part2)

public typealias ParsedStructure = [Int]

public func parse(_ input: String) -> ParsedStructure {
    input
        .split(separator: ",")
        .compactMap(pipe(String.init, Int.init))
}

func game(_ input: [Int], count: Int) -> Int {
    (1...count)
        .reduce((last: input.first ?? Int.min, numbers: [Int: [Int]]())) { t, turn in
            let next: Int
            if turn <= input.count {
                next = input[turn - 1]
            } else {
                if let l = t.numbers[t.last], l.count > 1 {
                    next = l[0] - l[1]
                } else {
                    next = 0
                }
            }
            
            var d = t.numbers
            d[next] = [turn] + (d[next]?.first.map({ [$0] }) ?? [])
            return (last: next, numbers: d)
        }
        .last
}

public func part1(_ parsedInput: ParsedStructure) {
    assert(game([0, 3, 6], count: 2020) == 436)
    print("Part 1: \(game(parsedInput, count: 2020))")
}
public func part2(_ parsedInput: ParsedStructure) { print("Part 2: \(game(parsedInput, count: 30000000))") }
