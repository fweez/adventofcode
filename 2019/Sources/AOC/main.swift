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

switch day {
/// To start a new day, copy and paste this case, updating to day x
case "day1":
    print("Day 1")
    day1.part1()
    day1.part2()
case "day2":
    print("Day 2")
    day2.part1()
    day2.part2()
default:
    assertionFailure("Unknown day \(day)")
}
