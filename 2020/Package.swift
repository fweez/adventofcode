// swift-tools-version:5.3
// The swift-tools-version declares the minimum version of Swift required to build this package.



import PackageDescription

let package = Package(
    name: "aoc20",
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
    ],
    dependencies: [
        .package(name: "Overture", url: "https://github.com/pointfreeco/swift-overture.git", from: "0.5.0"),
    ],
    targets: [
        .target(
            name: "AOCShared",
            dependencies: ["Overture"]),
        .target(
            name: "AOC",
            dependencies: ["AOCShared", "Overture", "day1", "day2", "day3", "day4"]),
        .testTarget(
            name: "AOCTests",
            dependencies: ["AOCShared", "Overture", "day1", "day2", "day3", "day4"]),
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
            dependencies: ["AOCShared", "Overture"])]
        + (5...24).map { i in
            .target(
                name: "day\(i)",
                dependencies: ["AOCShared", "Overture"],
                resources: [.process("input.txt")])
        }
)
