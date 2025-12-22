// swift-tools-version: 6.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "aoc2025",
    platforms: [.macOS(.v13)],
    products: [
        .executable(name: "aoc2025", targets: ["aoc2025"])
    ],
    targets: [
        .executableTarget(
            name: "aoc2025",
            path: ".",
            resources: [.copy("./input")]
        )
    ]
)
