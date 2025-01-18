// swift-tools-version: 6.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "SwiftGodotAoc2018",
    platforms: [.macOS(.v14)],
    products: [
        .library(
            name: "SwiftGodotAoc2018",
            type: .dynamic,
            targets: ["SwiftGodotAoc2018"]
        ),
    ],
    dependencies: [
        .package(url: "https://github.com/migueldeicaza/SwiftGodot", branch: "main"),
    ],
    targets: [
        .target(
            name: "SwiftGodotAoc2018",
            dependencies: [
                "SwiftGodot",
            ],
            path: "",
            sources: [
                "SwiftGodotAoc2018.swift",
                "lib",
                
                "day10/Day10.swift",
                "day11/Day11.swift",
                "day12/Day12.swift",
                "day13/Day13.swift",
                "day13/Day13Cart.swift",
                "day14/Day14.swift",
                "day15/Day15.swift",
                "day17/Day17.swift",
                "day18/Day18.swift",
                "day20/Day20.swift",
            ],

            swiftSettings: [
                .unsafeFlags(["-suppress-warnings"])
            ]
        ),
    ]
)
