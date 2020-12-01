import AOCShared
import Foundation
import Overture

/// To start a new day, copy and paste this file into a new dayX directory in Sources/
public func parse(_ input: String) -> [Int] {
    return ingestFile(input)
        .compactMap(pipe(String.init, Int.init))
        .sorted()
}

public func part1(_ parsedInput: [Int]) {
    let (a, b) = findSum(parsedInput)
    print("Part 1: \(a * b)")
}

public func part2(_ parsedInput: [Int]) {
    let (a, b, c) = findSum(parsedInput)
    print("Part 2: \(a * b * c)")
}

func findSum(_ input: [Int]) -> (Int, Int) {
    let r = input.reversed()
    return input.reduce((Int.min, Int.min)) { accum, v1 in
        if accum.0 != Int.min, accum.1 != Int.min, accum.0 + accum.1 == 2020 { return accum }
        return r.reduce((v1, Int.min)) { accum, v2 in
            if accum.0 != Int.min, accum.1 != Int.min, accum.0 + accum.1 == 2020 { return accum }
            return (accum.0, v2)
        }
    }
}

func findSum(_ input: [Int]) -> (Int, Int, Int) {
    let r = input.reversed()
    return input.reduce((Int.min, Int.min, Int.min)) { accum, v1 in
        if accum.0 != Int.min,
           accum.1 != Int.min,
           accum.2 != Int.min,
           accum.0 + accum.1 + accum.2 == 2020 { return accum }
        return r.reduce((v1, Int.min, Int.min)) { accum, v2 in
            if accum.0 != Int.min,
               accum.1 != Int.min,
               accum.2 != Int.min,
               accum.0 + accum.1 + accum.2 == 2020 { return accum }
            return input.reduce((v1, v2, Int.min)) { accum, v3 in
                if accum.0 != Int.min,
                   accum.1 != Int.min,
                   accum.2 != Int.min,
                   accum.0 + accum.1 + accum.2 == 2020 { return accum }
                return (accum.0, accum.1, v3)
            }
        }
    }
}
