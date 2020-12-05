import Foundation

import AOCShared

/// To start a new day, copy and paste this line, updating to day x
import day1
import day2
import day3
import day4

guard let day = CommandLine.arguments.last else { preconditionFailure("You must specify a day to run") }

func run(_ day: String, _ p1: () -> Void, _ p2: () -> Void) -> Void {
    print("Day \(day)")
    print("Part 1 completed in \(time(p1))s")
    print("Part 2 completed in \(time(p2))s")
}

func run<A>(_ day: String, _ parse: (String) -> A, _ p1: (A) -> Void, _ p2: (A) -> Void) -> Void {
    print("Day \(day)")
    let inputFileName = "day\(day).txt"
    var parsedInput: A?
    print("Input file parsed in \(time({ parsedInput = parse(inputFileName) }))s")
    print("Part 1 completed in \(time({ p1(parsedInput!) }))")
    print("Part 2 completed in \(time({ p2(parsedInput!) }))")
}

switch day {
case "day1": run("1", day1.parse, day1.part1, day1.part2)
case "day2": run("2", day2.parse, day2.part1, day2.part2)
case "day3": run("3", day3.parse, day3.part1, day3.part2)
case "day4": run("4", day4.parse, day4.part1, day4.part2)
default:
    run("1", day1.parse, day1.part1, day1.part2)
}



