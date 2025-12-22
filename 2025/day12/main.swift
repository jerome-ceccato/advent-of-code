import Foundation

struct Area {
    let width: Int
    let height: Int
    let presents: [Int]
}

struct Shape {
    let representation: [[Bool]]
    let footprint: Int
}

struct Gifts {
    let shapes: [Shape]
    let areas: [Area]
}

func parseShape(raw: String) -> Shape {
    let lines = raw.components(separatedBy: "\n")
    let representation = lines[1...].map { line in line.map { c in c == "#" } }
    let footprint = representation.reduce(0) { acc, line in acc + line.count { $0 } }
    return Shape(representation: representation, footprint: footprint)
}

func parseArea(raw: String) -> Area {
    let parts = raw.components(separatedBy: ": ")
    let dimensions = parts[0].components(separatedBy: "x").compactMap(Int.init)

    return Area(
        width: dimensions[0],
        height: dimensions[1],
        presents: parts[1].components(separatedBy: " ").compactMap(Int.init)
    )
}

func parseInput(path: String = "input") -> Gifts {
    let path = Bundle.module.path(forResource: "input", ofType: nil)!
    let contents = try! String(contentsOfFile: path, encoding: .utf8)

    var parts = contents.components(separatedBy: "\n\n")
    let areas = parts.popLast()!

    return Gifts(
        shapes: parts.map(parseShape(raw:)),
        areas: areas.components(separatedBy: "\n").map(parseArea(raw:))
    )
}

func estimateFit(shapes: [Shape], area: Area) -> Bool {
    let totalShapeFootprint = area.presents.enumerated().map { (i, amount) in
        shapes[i].footprint * amount
    }.reduce(0, +)

    if totalShapeFootprint > (area.width * area.height) {
        return false
    }

    let simpleShapePacking = (area.width / 3) * (area.height / 3)
    let numberOfPresents = area.presents.reduce(0, +)
    if numberOfPresents <= simpleShapePacking {
        return true
    }

    fatalError("Area \(area): Cannot easily decide")
}

let data = parseInput()
print(data.areas.count { estimateFit(shapes: data.shapes, area: $0) })
