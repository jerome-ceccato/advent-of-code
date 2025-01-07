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
            path: ".",
            swiftSettings: [
//                .swiftLanguageMode(.v5),
                .unsafeFlags(["-suppress-warnings"])
            ]
        ),
    ]
)
