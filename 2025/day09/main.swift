import Foundation

struct Point: Equatable, Hashable {
    let x: Int
    let y: Int

    static let zero = Point(x: 0, y: 0)

    static func + (lhs: Point, rhs: Point) -> Point {
        Point(x: lhs.x + rhs.x, y: lhs.y + rhs.y)
    }

    func area(with other: Point) -> Int {
        (abs(x - other.x) + 1) * (abs(y - other.y) + 1)
    }
}

struct Rectangle {
    let a: Point
    let b: Point
    let area: Int
}

enum Edge {
    case horizontal(x1: Int, x2: Int, y: Int)
    case vertical(x: Int, y1: Int, y2: Int)

    init(from a: Point, and b: Point) {
        if a.x == b.x {
            self = .vertical(x: a.x, y1: min(a.y, b.y), y2: max(a.y, b.y))
        } else {
            self = .horizontal(x1: min(a.x, b.x), x2: max(a.x, b.x), y: a.y)
        }
    }

    func intersectsRaycastedPoint(_ point: Point) -> Bool {
        // assume raycast going right (half-open rule)
        switch self {
        case .horizontal:
            return false
        case .vertical(let x, let y1, let y2):
            return point.x <= x && point.y >= y1 && point.y < y2
        }
    }
}

func parseInput(path: String = "input") -> [Point] {
    let path = Bundle.module.path(forResource: "input", ofType: nil)!
    let contents = try! String(contentsOfFile: path, encoding: .utf8)

    return contents.components(separatedBy: "\n").map { line in
        let coords = line.components(separatedBy: ",").compactMap(Int.init)
        return Point(x: coords[0], y: coords[1])
    }
}

func sortedRectangles(point: [Point]) -> [Rectangle] {
    var rectangles = [Rectangle]()
    for i in 0..<points.count {
        for j in (i + 1)..<points.count {
            rectangles.append(
                Rectangle(a: points[i], b: points[j], area: points[i].area(with: points[j])))
        }
    }

    rectangles.sort { lhs, rhs in lhs.area > rhs.area }
    return rectangles
}

func makeEdges(points: [Point]) -> [Edge] {
    return (0..<points.count).map { i in
        Edge(from: points[i], and: points[(i + 1) % points.count])
    }
}

func raycast(point: Point, edges: [Edge]) -> Int {
    edges.count { $0.intersectsRaycastedPoint(point) }
}

func isPoint(_ point: Point, inside circuit: [Edge], cache: inout [Point: Bool]) -> Bool {
    if let cached = cache[point] {
        return cached
    }

    let result = (raycast(point: point, edges: circuit) % 2) == 1
    cache[point] = result
    return result
}

func isRectangle(_ rect: Rectangle, inside circuit: [Edge], cache: inout [Point: Bool]) -> Bool {
    let topLeft = Point(x: min(rect.a.x, rect.b.x), y: min(rect.a.y, rect.b.y))
    let bottomRight = Point(x: max(rect.a.x, rect.b.x), y: max(rect.a.y, rect.b.y))

    for x in topLeft.x...bottomRight.x {
        if !isPoint(Point(x: x, y: topLeft.y), inside: circuit, cache: &cache)
            || !isPoint(Point(x: x, y: bottomRight.y), inside: circuit, cache: &cache)
        {
            return false
        }
    }

    for y in topLeft.y...bottomRight.y {
        if !isPoint(Point(x: topLeft.x, y: y), inside: circuit, cache: &cache)
            || !isPoint(Point(x: bottomRight.x, y: y), inside: circuit, cache: &cache)
        {
            return false
        }
    }

    return true
}

func register(edges: [Edge], inside cache: inout [Point: Bool]) {
    for edge in edges {
        switch edge {
        case .horizontal(let x1, let x2, let y):
            for x in x1...x2 {
                cache[Point(x: x, y: y)] = true
            }
        case .vertical(let x, let y1, let y2):
            for y in y1...y2 {
                cache[Point(x: x, y: y)] = true
            }
        }
    }
}

let points = parseInput()
let rectangles = sortedRectangles(point: points)
print(rectangles[0].area)

let edges = makeEdges(points: points)
var cache = [Point: Bool]()
register(edges: edges, inside: &cache)
let bestRect = rectangles.first { isRectangle($0, inside: edges, cache: &cache) }
print(bestRect!.area)
