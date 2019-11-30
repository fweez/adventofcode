import Foundation
import day0

print("Hola, world")

guard let day = CommandLine.arguments.first else { assertionFailure("You must specify a day to run") }

switch day {
case "day0":
    day0.part1()
    day0.part2()
default:
    assertionFailure("Unknown day \(day)")
}
