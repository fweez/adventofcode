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

switch day {
/// To start a new day, copy and paste this case, updating to day x
case "day1": run("1", day1.part1, day1.part2)
case "day2": run("2", day2.part1, day2.part2)
case "day3": run("3", day3.part1, day3.part2)
case "day4": run("4", day4.part1, day4.part2)

default:
    preconditionFailure("Unknown day \(day)")
}



