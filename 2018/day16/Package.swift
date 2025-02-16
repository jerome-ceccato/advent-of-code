// swift-tools-version: 6.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "day16",
    platforms: [.macOS(.v15)],
    products: [
        .executable(name: "day16", targets: ["day16"]),
    ],
    dependencies: [
        .package(path: "../Device"),
    ],
    targets: [
        .executableTarget(
            name: "day16",
            dependencies: ["Device"],
            path: ".",
            resources: [.copy("./input")]
        ),
    ]
)
