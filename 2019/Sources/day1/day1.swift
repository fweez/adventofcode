import AOCShared
import Foundation

/// To start a new day, copy and paste this file into a new dayX directory in Sources/
public func part1() { print("Part 1: \(calculateModuleFuel())") }
public func part2() { print("Part 2: \(calculateTotalFuel())") }

func calculateModuleFuel() -> Int {
    ingestFile("day1.txt")
        .map { Int(String($0))! }
        .map(calculateModuleFuel)
        .reduce(0, +)
}

func calculateTotalFuel() -> Int {
    ingestFile("day1.txt")
        .map { Int(String($0))! }
        .map(calculateTotalFuel)
        .reduce(0, +)
}

func calculateModuleFuel(_ module: Int) -> Int { Int(floor(Double(module) / 3.0)) - 2 }

func calculateTotalFuel(_ module: Int) -> Int {
    guard module >= 2 else { return 0 }
    let f = calculateModuleFuel(module)
    let ff = calculateTotalFuel(f)
    guard ff > 0 else { return f }
    return f + ff
}
