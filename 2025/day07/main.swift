import Foundation

struct Point: Equatable, Hashable {
    let x: Int
    let y: Int

    static let zero = Point(x: 0, y: 0)
    static let down = Point(x: 0, y: 1)
    static let left = Point(x: -1, y: 0)
    static let right = Point(x: 1, y: 0)

    static func + (lhs: Point, rhs: Point) -> Point {
        Point(x: lhs.x + rhs.x, y: lhs.y + rhs.y)
    }
}

struct Size: Equatable, Hashable {
    let w: Int
    let h: Int

    func contains(point: Point) -> Bool {
        point.x >= 0 && point.x < w && point.y >= 0 && point.y < h
    }
}

struct World {
    let start: Point
    let splitters: Set<Point>
    let size: Size
}

func parseInput(path: String = "input") -> World {
    let path = Bundle.module.path(forResource: "input", ofType: nil)!
    let contents = try! String(contentsOfFile: path, encoding: .utf8)

    let lines = contents.components(separatedBy: "\n")
    var start = Point.zero
    var splitters = Set<Point>()

    for (y, l) in lines.enumerated() {
        for (x, c) in l.enumerated() {
            if c == "S" {
                start = Point(x: x, y: y)
            } else if c == "^" {
                splitters.insert(Point(x: x, y: y))
            }
        }
    }

    return World(
        start: start,
        splitters: splitters,
        size: Size(w: lines[0].count, h: lines.count)
    )
}

func countSplits(world: World) -> Int {
    var queue: [Point] = [world.start]
    var visited: Set<Point> = []
    var splits = 0

    while let current = queue.popLast() {
        guard !visited.contains(current), world.size.contains(point: current) else {
            continue
        }

        visited.insert(current)
        if world.splitters.contains(current) {
            queue.append(current + .left)
            queue.append(current + .right)
            splits += 1
        } else {
            queue.append(current + .down)
        }
    }

    return splits
}

func traverse(world: World, start: Point, cache: inout [Point: Int]) -> Int {
    if let hit = cache[start] {
        return hit
    }

    var current = start
    while world.size.contains(point: current) {
        if world.splitters.contains(current) {
            let result =
                traverse(world: world, start: current + .left, cache: &cache)
                + traverse(world: world, start: current + .right, cache: &cache)
            cache[start] = result
            return result
        }
        current = current + .down
    }

    cache[start] = 1
    return 1
}

func quantumTraverse(world: World) -> Int {
    var cache = [Point: Int]()
    return traverse(world: world, start: world.start, cache: &cache)
}

let world = parseInput()
print(countSplits(world: world))
print(quantumTraverse(world: world))
