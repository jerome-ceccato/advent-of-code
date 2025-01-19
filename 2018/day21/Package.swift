// swift-tools-version: 6.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "day21",
    platforms: [.macOS(.v15)],
    products: [
        .executable(name: "day21", targets: ["day21"]),
    ],
    dependencies: [
        .package(path: "../Device"),
    ],
    targets: [
        .executableTarget(
            name: "day21",
            dependencies: ["Device"],
            path: ".",
            resources: [.copy("./input")]
        ),
    ]
)
