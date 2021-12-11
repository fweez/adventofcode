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
        .package(name: "swift-parsing", url: "https://github.com/pointfreeco/swift-parsing", .branch("main")),
        .package(url: "https://github.com/apple/swift-collections.git", .upToNextMajor(from: "0.0.5")),
        .package(url: "https://github.com/apple/swift-algorithms", from: "1.0.0"),
    ],
    targets: [
        .target(
            name: "AOCShared",
            dependencies: ["Overture"]),
    ]
        + (1...24).map { i in
            .target(
                name: "day\(i)",
                dependencies: [
                    "Overture",
                    "AOCShared",
                    .product(name:"Parsing", package:"swift-parsing"),
                    .product(name: "Collections", package: "swift-collections"),
                    .product(name: "Algorithms", package: "swift-algorithms"),
                ],
                resources: [.process("input.txt")])
        }
)
