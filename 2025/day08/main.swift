import Foundation

private func sq(_ a: Int) -> Int { a * a }

struct Point: Equatable, Hashable {
    let x: Int
    let y: Int
    let z: Int

    static let zero = Point(x: 0, y: 0, z: 0)

    func distance(to other: Point) -> Double {
        sqrt(Double(sq(x - other.x) + sq(y - other.y) + sq(z - other.z)))
    }
}

func parseInput(path: String = "input") -> Set<Point> {
    let path = Bundle.module.path(forResource: "input", ofType: nil)!
    let contents = try! String(contentsOfFile: path, encoding: .utf8)

    return Set(
        contents.components(separatedBy: "\n").map { line in
            let items = line.components(separatedBy: ",").compactMap(Int.init)
            return Point(x: items[0], y: items[1], z: items[2])
        }
    )
}

struct Pair {
    let lhs: Point
    let rhs: Point
    let distance: Double
}

func sortedPairs(points: Set<Point>) -> [Pair] {
    var result = [Pair]()
    let raw = Array(points)
    for i in 0..<raw.count {
        for j in (i + 1)..<raw.count {
            result.append(Pair(lhs: raw[i], rhs: raw[j], distance: raw[i].distance(to: raw[j])))
        }
    }
    result.sort { lhs, rhs in lhs.distance < rhs.distance }
    return result
}

func mergeOrInsert(in circuits: inout [Set<Point>], i: Int, new: Point) -> Bool {
    for j in 0..<circuits.count {
        if circuits[j].contains(new) {
            if i == j {
                // print("Point \(new) already in \(circuits[i])")
                return false
            }

            // print("Merging \(circuits[i]) and \(circuits[j])")
            circuits[i].formUnion(circuits[j])
            circuits.remove(at: j)
            return true
        }
    }
    // print("Inserting \(new) in \(circuits[i])")
    return circuits[i].insert(new).inserted
}

@discardableResult
func insert(in circuits: inout [Set<Point>], pair: Pair) -> Bool {
    for i in 0..<circuits.count {
        if circuits[i].contains(pair.lhs) {
            return mergeOrInsert(in: &circuits, i: i, new: pair.rhs)
        } else if circuits[i].contains(pair.rhs) {
            return mergeOrInsert(in: &circuits, i: i, new: pair.lhs)
        }
    }
    // print("New circuit with pair \(pair)")
    circuits.append(Set<Point>([pair.lhs, pair.rhs]))
    return true
}

func insert(in circuits: inout [Set<Point>], point: Point) {
    for circuit in circuits {
        if circuit.contains(point) {
            return
        }
    }
    circuits.append(Set<Point>([point]))
}

func buildCircuits(points: Set<Point>, pairs: [Pair], limit: Int) -> [Set<Point>] {
    var circuits = [Set<Point>]()

    for pair in pairs.prefix(limit) {
        insert(in: &circuits, pair: pair)
    }
    for point in points {
        insert(in: &circuits, point: point)
    }

    circuits.sort { lhs, rhs in lhs.count > rhs.count }
    return circuits
}

func findLastConnectionPair(points: Set<Point>, pairs: [Pair]) -> Pair {
    var circuits = [Set<Point>]()

    for pair in pairs {
        insert(in: &circuits, pair: pair)
        if circuits.count == 1 && circuits[0].count == points.count {
            return pair
        }
    }
    fatalError()
}

let points = parseInput()
let sortedConnections = sortedPairs(points: points)
let circuits = buildCircuits(points: points, pairs: sortedConnections, limit: 1000)
let total = circuits.prefix(3).map { $0.count }.reduce(1, *)
print(total)

let lastConnection = findLastConnectionPair(points: points, pairs: sortedConnections)
print(lastConnection.lhs.x * lastConnection.rhs.x)
