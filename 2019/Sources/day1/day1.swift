import AOCShared
import Overture
import Foundation

/// To start a new day, copy and paste this file into a new dayX directory in Sources/
public func part1() { print("Part 1: \(calculateFuel(stringToModuleFuel))") }
public func part2() { print("Part 2: \(calculateFuel(stringToTotalFuel))") }

func calculateFuel(_ f: (String.SubSequence) -> Int) -> Int {
    ingestFile("day1.txt")
        .map(f)
        .reduce(0, +)
}

let intify: (String.SubSequence) -> Int = pipe(String.init, Int.init, force)

let stringToModuleFuel = pipe(intify, calculateModuleFuel)
let stringToTotalFuel = pipe(intify, calculateTotalFuel)

let calculateModuleFuel: (Int) -> Int =
    pipe(Double.init,
         { d -> Double in d / 3.0 },
         floor,
         Int.init,
         { i -> Int in i - 2 })

func calculateTotalFuel(_ module: Int) -> Int {
    guard module >= 2 else { return 0 }
    let f = calculateModuleFuel(module)
    let ff = calculateTotalFuel(f)
    guard ff > 0 else { return f }
    return f + ff
}

