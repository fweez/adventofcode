import AOCShared
import Overture
import Foundation

let calculateModuleFuel: (Int) -> Int = pipe(
    Double.init,
    divideBy(3.0),
    floor,
    Int.init,
    subtractBy(2))

func calculateTotalFuel(_ module: Int) -> Int {
    guard module >= 2 else { return 0 }
    let f = calculateModuleFuel(module)
    let ff = calculateTotalFuel(f)
    guard ff > 0 else { return f }
    return f + ff
}

func calculateFuel(_ f: (String.SubSequence) -> Int) -> Int {
    ingestFile("day1.txt")
        .map(f)
        .reduce(0, +)
}

let stringToModuleFuel = pipe(intify, calculateModuleFuel)
let stringToTotalFuel = pipe(intify, calculateTotalFuel)

/// To start a new day, copy and paste this file into a new dayX directory in Sources/
public func part1() { print("Part 1: \(calculateFuel(stringToModuleFuel))") }
public func part2() { print("Part 2: \(calculateFuel(stringToTotalFuel))") }
