import Foundation
/// To start a new day, copy and paste this line, updating to day x
import day0
import day1

guard let day = CommandLine.arguments.last else { preconditionFailure("You must specify a day to run") }

switch day {
/// To start a new day, copy and paste this case, updating to day x
case "day0":
    print("Day 0")
    day0.part1()
    day0.part2()
case "day1":
    print("Day 1")
    day1.part1()
    day1.part2()
default:
    assertionFailure("Unknown day \(day)")
}
