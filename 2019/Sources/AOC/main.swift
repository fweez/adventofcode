import Foundation
/// To start a new day, copy and paste this line, updating to day x
import day1
import day2
import day3
import day4
import day5
import day6
import day7
import day8
import day9
import day10
import day11
import day12
import day13
import day14
import day15
import day16
import day17
import day18
import day19
import day20
import day21
import day22
import day23
import day24
import day25

guard let day = CommandLine.arguments.last else { preconditionFailure("You must specify a day to run") }

let nf = NumberFormatter()
nf.maximumFractionDigits = 5

func time(_ f: () -> Void) -> String {
    let start = Date()
    f()
    let elapsed = Date().timeIntervalSince(start)
    return nf.string(for: elapsed) ?? "Couldn't format time \(elapsed)"
}

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
case "day1": run("1", day1.part1, day1.part2)
case "day2": run("2", day2.part1, day2.part2)
case "day3": run("3", day3.part1, day3.part2)
case "day4": run("4", day4.part1, day4.part2)
case "day5": run("5", day5.part1, day5.part2)
case "day6": run("6", day6.part1, day6.part2)
case "day7": run("7", day7.part1, day7.part2)
case "day8": run("8", day8.parse, day8.part1, day8.part2)
case "day9": run("9", day9.parse, day9.part1, day9.part2)
case "day10": run("10", day10.parse, day10.part1, day10.part2)
case "day11": run("11", day11.parse, day11.part1, day11.part2)
case "day12": run("12", day12.parse, day12.part1, day12.part2)
case "day13": run("13", day13.parse, day13.part1, day13.part2)
case "day14": run("14", day14.parse, day14.part1, day14.part2)

default:
    preconditionFailure("Unknown day \(day)")
}



