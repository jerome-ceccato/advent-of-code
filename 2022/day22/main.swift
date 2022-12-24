import Foundation

extension String {
    public subscript(_ idx: Int) -> Character {
        self[self.index(self.startIndex, offsetBy: idx)]
    }
}

enum BoardTile: Character {
    case empty = " "
    case ground = "."
    case wall = "#"
}

enum PathAction {
    case forward(n: Int)
    case left
    case right
}

enum Direction: Int {
    case R, D, L, U
    
    func next() -> Direction {
        Direction(rawValue: (rawValue + 1) % 4)!
    }
    
    func previous() -> Direction {
        Direction(rawValue: (rawValue + 3) % 4)!
    }
    
    func reverse() -> Direction {
        Direction(rawValue: (rawValue + 2) % 4)!
    }
}

struct CubeEdge {
    let id: Int
    let edge: Direction
}

struct Wrap {
    let first: CubeEdge
    let second: CubeEdge
    let reverse: Bool
}

struct Point {
    let x: Int
    let y: Int
    
    static func +(rhs: Point, lhs: Point) -> Point {
        return Point(x: rhs.x + lhs.x, y: rhs.y + lhs.y)
    }
}

struct PointRange {
    let x: Range<Int>
    let y: Range<Int>
}

class Map {
    let board: [[BoardTile]]
    let path: [PathAction]
    var position: Point
    var facing = Direction.R
    
    init(board: [[BoardTile]], path: [PathAction], position: Point) {
        self.board = board
        self.path = path
        self.position = position
    }
    
    let cubeRepresentation: [Wrap] = [
        Wrap(first: CubeEdge(id: 1, edge: .U), second: CubeEdge(id: 6, edge: .L), reverse: false),
        Wrap(first: CubeEdge(id: 1, edge: .L), second: CubeEdge(id: 5, edge: .L), reverse: true),
        Wrap(first: CubeEdge(id: 2, edge: .U), second: CubeEdge(id: 6, edge: .D), reverse: false),
        Wrap(first: CubeEdge(id: 2, edge: .R), second: CubeEdge(id: 4, edge: .R), reverse: true),
        Wrap(first: CubeEdge(id: 2, edge: .D), second: CubeEdge(id: 3, edge: .R), reverse: false),
        Wrap(first: CubeEdge(id: 3, edge: .L), second: CubeEdge(id: 5, edge: .U), reverse: false),
        Wrap(first: CubeEdge(id: 4, edge: .D), second: CubeEdge(id: 6, edge: .R), reverse: false),
    ]
    
    let cubeRanges: [Int: PointRange] = [
        1: PointRange(x: 50 ..< 100, y: 0 ..< 50),
        2: PointRange(x: 100 ..< 150, y: 0 ..< 50),
        3: PointRange(x: 50 ..< 100, y: 50 ..< 100),
        4: PointRange(x: 50 ..< 100, y: 100 ..< 150),
        5: PointRange(x: 0 ..< 50, y: 100 ..< 150),
        6: PointRange(x: 0 ..< 50, y: 150 ..< 200),
    ]
}

func parseInput(filename: String) -> Map {
    let contents = try! String(contentsOfFile: Bundle.main.path(forResource: filename, ofType: nil)!, encoding: .utf8)
    let parts = contents.components(separatedBy: "\n\n")
    
    let board = parts[0]
        .components(separatedBy: "\n")
        .filter { !$0.isEmpty }
        .map { line in line.compactMap(BoardTile.init) }
    
    let path: [PathAction] = Array(parts[1].trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "R").map { withoutR in
        Array(withoutR.components(separatedBy: "L").map { numbers in
            [PathAction.forward(n: Int(numbers)!)]
        }.joined(separator: [PathAction.left]))
    }.joined(separator: [PathAction.right]))
    
    let posX = board[0].firstIndex(of: .ground)!
    return Map(board: board, path: path, position: Point(x: posX, y: 0))
}

extension Map {
    func boardAt(_ point: Point) -> BoardTile {
        return board.indices.contains(point.y) ? (board[point.y].indices.contains(point.x) ? board[point.y][point.x] : .empty) : .empty
    }
    
    func nextOffset() -> Point {
        switch facing {
        case .R:
            return Point(x: 1, y: 0)
        case .L:
            return Point(x: -1, y: 0)
        case .D:
            return Point(x: 0, y: 1)
        case .U:
            return Point(x: 0, y: -1)
        }
    }
    
    func nextIndexFlat() -> Point {
        let target = position + nextOffset()
        if (boardAt(target) != .empty) {
            return target
        } else {
            switch facing {
            case .R, .L:
                let x = facing == .R ? board[position.y].firstIndex { $0 != .empty } : board[position.y].lastIndex { $0 != .empty }
                return Point(x: x!, y: position.y)
                
            case .D, .U:
                let y = facing == .D ? board.firstIndex { ($0.indices.contains(position.x) ? $0[position.x] : .empty) != .empty }
                : board.lastIndex { ($0.indices.contains(position.x) ? $0[position.x] : .empty) != .empty }
                return Point(x: position.x, y: y!)
            }
        }
    }
    
    func faceId(for position: Point) -> Int? {
        for (key, range) in cubeRanges {
            if range.x.contains(position.x) && range.y.contains(position.y) {
                return key
            }
        }
        return nil
    }
    
    func performWrap(from: CubeEdge, to: CubeEdge, reverse: Bool) -> (Point, Direction) {
        var relevantCoord: Int = {
            switch from.edge {
            case .L, .R:
                return position.y % 50
            case .D, .U:
                return position.x % 50
            }
        }()
        
        if reverse {
            relevantCoord = 49 - relevantCoord
        }
        
        let newPoint: Point = {
            let cube = cubeRanges[to.id]!
            switch to.edge {
            case .L:
                return Point(x: cube.x.lowerBound, y: cube.y.lowerBound + relevantCoord)
            case .R:
                return Point(x: cube.x.upperBound - 1, y: cube.y.lowerBound + relevantCoord)
            case .U:
                return Point(x: cube.x.lowerBound + relevantCoord, y: cube.y.lowerBound)
            case .D:
                return Point(x: cube.x.lowerBound + relevantCoord, y: cube.y.upperBound - 1)
            }
        }()
        
        return (newPoint, to.edge.reverse())
    }
    
    func nextStepCube() -> (Point, Direction) {
        let target = position + nextOffset()
        // We can reach the same face or another immediately adjacent face, keep going
        if faceId(for: target) != nil {
            return (target, facing)
        } else {
            let currentFace = faceId(for: position)!
            for wrap in cubeRepresentation {
                if wrap.first.id == currentFace && wrap.first.edge == facing {
                    return performWrap(from: wrap.first, to: wrap.second, reverse: wrap.reverse)
                } else if wrap.second.id == currentFace && wrap.second.edge == facing {
                    return performWrap(from: wrap.second, to: wrap.first, reverse: wrap.reverse)
                }
            }
            fatalError("No wrap point available")
        }
    }
    
    func nextStep(insideCube: Bool) -> (Point, Direction) {
        return insideCube ? nextStepCube() : (nextIndexFlat(), facing)
    }
    
    func moveForward(steps: Int, insideCube: Bool) {
        for _ in 0 ..< steps {
            let step = nextStep(insideCube: insideCube)
            if boardAt(step.0) == .ground {
                position = step.0
                facing = step.1
            } else if boardAt(step.0) == .wall {
                return
            } else {
                fatalError("Failed to get board position")
            }
        }
    }
    
    func run(insideCube: Bool) -> Int {
        path.forEach { action in
            switch action {
            case .forward(let n):
                moveForward(steps: n, insideCube: insideCube)
            case .right:
                facing = facing.next()
            case .left:
                facing = facing.previous()
            }
        }
        return 1000 * (position.y + 1) + 4 * (position.x + 1) + facing.rawValue
    }
}

print(parseInput(filename: "input").run(insideCube: false))
print(parseInput(filename: "input").run(insideCube: true))
