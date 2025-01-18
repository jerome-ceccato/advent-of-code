// swift-tools-version: 6.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "day19",
    platforms: [.macOS(.v15)],
    products: [
        .executable(name: "day19", targets: ["day19"]),
    ],
    dependencies: [
        .package(path: "../Device"),
    ],
    targets: [
        .executableTarget(
            name: "day19",
            dependencies: ["Device"],
            path: ".",
            resources: [.copy("./input")]
        ),
    ]
)
