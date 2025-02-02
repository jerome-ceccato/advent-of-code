// swift-tools-version: 6.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "aoc2017",
    platforms: [.macOS(.v15)],
    products: [
        .executable(name: "aoc2017", targets: ["aoc2017"])
    ],
    targets: [
        .executableTarget(
            name: "aoc2017",
            path: ".",
            resources: [.copy("./input")]
        )
    ]
)
