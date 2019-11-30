// swift-tools-version:5.1
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "aoc19",
    products: [
        .library(
            name: "AOCShared",
            targets: ["AOCShared"]),
        
        /// To start a day, copy and paste this target, updating the name to "dayX"
        .library(
            name: "day0",
            targets: ["day0"])
    ],
    dependencies: [
        .package(url: "https://github.com/pointfreeco/swift-overture.git", from: "0.5.0"),
        .package(url: "https://github.com/pointfreeco/swift-prelude.git", .branch("master")),
    ],
    targets: [
        .target(
            name: "AOCShared",
            dependencies: []),
        
        /// To start a day, copy and paste this target, updating the name to "dayX"
        .target(
            name: "day0",
            dependencies: ["AOCShared", "Overture", "Prelude"]),
        
        .target(
            name: "AOC",
            /// To start a day, add a "dayX" dependency here
            dependencies: ["AOCShared", "day0"]),
        .testTarget(
            name: "AOCTests",
            /// To start a day, add a "dayX" dependency here
            dependencies: ["day0"]),
    ]
)
