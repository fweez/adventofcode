// swift-tools-version:5.3
// The swift-tools-version declares the minimum version of Swift required to build this package.



import PackageDescription

let package = Package(
    name: "aoc21",
    products: [
        .library(
            name: "AOCShared",
            targets: ["AOCShared"]),
        
    ],
    dependencies: [
        .package(name: "Overture", url: "https://github.com/pointfreeco/swift-overture.git", from: "0.5.0"),
    ],
    targets: [
        .target(
            name: "AOCShared",
            dependencies: ["Overture"])
    ]
        + (1...24).map { i in
            .target(
                name: "day\(i)",
                dependencies: ["AOCShared", "Overture"],
                resources: [.process("input.txt")])
        }
)
