// swift-tools-version:5.1
// The swift-tools-version declares the minimum version of Swift required to build this package.



import PackageDescription

let package = Package(
    name: "aoc19",
    products: [
        .library(
            name: "AOCShared",
            targets: ["AOCShared"]),
        .library(
            name: "day1",
            targets: ["day1"]),
        .library(
            name: "day2",
            targets: ["day2"]),
        .library(
            name: "day3",
            targets: ["day3"]),
        .library(
            name: "day4",
            targets: ["day4"]),
        .library(
            name: "day5",
            targets: ["day5"]),
        .library(
            name: "day6",
            targets: ["day6"]),
        .library(
            name: "day7",
            targets: ["day7"]),
        .library(
            name: "day8",
            targets: ["day8"]),
        .library(
            name: "day9",
            targets: ["day9"]),
        .library(
            name: "day10",
            targets: ["day10"]),
        .library(
            name: "day11",
            targets: ["day11"]),
        .library(
            name: "day12",
            targets: ["day12"]),
        .library(
            name: "day13",
            targets: ["day13"]),
        .library(
            name: "day14",
            targets: ["day14"]),
        .library(
            name: "day15",
            targets: ["day15"]),
        .library(
            name: "day16",
            targets: ["day16"]),
        .library(
            name: "day17",
            targets: ["day17"]),
        .library(
            name: "day18",
            targets: ["day18"]),
        .library(
            name: "day19",
            targets: ["day19"]),
        .library(
            name: "day20",
            targets: ["day20"]),
        .library(
            name: "day21",
            targets: ["day21"]),
        .library(
            name: "day22",
            targets: ["day22"]),
        .library(
            name: "day23",
            targets: ["day23"]),
        .library(
            name: "day24",
            targets: ["day24"]),
        .library(
            name: "day25",
            targets: ["day25"]),
    ],
    dependencies: [
        .package(url: "https://github.com/pointfreeco/swift-overture.git", from: "0.5.0"),
    ],
    targets: [
        .target(
            name: "AOCShared",
            dependencies: ["Overture"]),
        .target(
            name: "AOC",
            dependencies: ["AOCShared", "Overture", "day1", "day2", "day3", "day4", "day5", "day6", "day7", "day8", "day9", "day10", "day11", "day12", "day13", "day14", "day15", "day16", "day17", "day18", "day19", "day20", "day21", "day22", "day23", "day24", "day25"]),
        .target(
            name: "AOCday7",
            dependencies: ["day7"]),
        .testTarget(
            name: "AOCTests",
            dependencies: ["AOCShared", "Overture", "day1", "day2", "day3", "day4", "day5", "day6", "day7", "day8", "day9", "day10", "day11", "day12", "day13", "day14", "day15", "day16", "day17", "day18", "day19", "day20", "day21", "day22", "day23", "day24", "day25"]),
        .target(
            name: "day1",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day2",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day3",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day4",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day5",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day6",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day7",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day8",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day9",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day10",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day11",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day12",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day13",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day14",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day15",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day16",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day17",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day18",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day19",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day20",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day21",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day22",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day23",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day24",
            dependencies: ["AOCShared", "Overture"]),
        .target(
            name: "day25",
            dependencies: ["AOCShared", "Overture"]),
        
    ]
)
