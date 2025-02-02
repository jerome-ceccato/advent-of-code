import Foundation

enum Direction: String {
    case north = "n"
    case northEast = "ne"
    case northWest = "nw"
    case south = "s"
    case southEast = "se"
    case southWest = "sw"

    // https://www.redblobgames.com/grids/hexagons/#coordinates-axial
    var hexOffset: HexCoord {
        switch self {
        case .north:
            return HexCoord(q: 0, r: -1)
        case .northEast:
            return HexCoord(q: 1, r: -1)
        case .northWest:
            return HexCoord(q: -1, r: 0)
        case .south:
            return HexCoord(q: 0, r: 1)
        case .southEast:
            return HexCoord(q: 1, r: 0)
        case .southWest:
            return HexCoord(q: -1, r: 1)
        }
    }
}

struct HexCoord: Equatable, Hashable {
    var q: Int
    var r: Int

    var s: Int {
        -q - r
    }

    func distance(to other: HexCoord) -> Int {
        let normalized = self - other
        return (abs(normalized.q) + abs(normalized.r) + abs(normalized.s)) / 2
    }

    static let zero = HexCoord(q: 0, r: 0)

    static func + (lhs: HexCoord, rhs: HexCoord) -> HexCoord {
        HexCoord(q: lhs.q + rhs.q, r: lhs.r + rhs.r)
    }

    static func - (lhs: HexCoord, rhs: HexCoord) -> HexCoord {
        HexCoord(q: lhs.q - rhs.q, r: lhs.r - rhs.r)
    }

    static func + (lhs: HexCoord, rhs: Direction) -> HexCoord {
        lhs + rhs.hexOffset
    }
}

func readInput() -> [Direction] {
    let path = Bundle.module.path(forResource: "input", ofType: nil)!
    let contents = try! String(contentsOfFile: path, encoding: .utf8)

    return contents.components(separatedBy: ",").compactMap(Direction.init(rawValue:))
}

func farthest(directions: [Direction]) -> Int {
    let coords = directions.reduce(into: [HexCoord.zero]) { acc, dir in
        acc.append(acc.last! + dir)
    }
    let distances = coords.map { $0.distance(to: .zero) }
    return distances.max()!
}

let directions = readInput()
let destination = directions.reduce(HexCoord.zero, +)
print(destination.distance(to: .zero))
print(farthest(directions: directions))
