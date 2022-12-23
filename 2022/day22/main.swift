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
}

struct Point {
    let x: Int
    let y: Int
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
    
    func nextIndex() -> Point {
        switch facing {
        case .R, .L:
            let offset = facing == .R ? 1 : -1
            let target = Point(x: position.x + offset, y: position.y)
            if (boardAt(target) != .empty) {
                return target
            } else {
                let x = facing == .R ? board[position.y].firstIndex { $0 != .empty } : board[position.y].lastIndex { $0 != .empty }
                return Point(x: x!, y: position.y)
            }

        case .D, .U:
            let offset = facing == .D ? 1 : -1
            let target = Point(x: position.x, y: position.y + offset)
            if (boardAt(target) != .empty) {
                return target
            } else {
                let y = facing == .D ? board.firstIndex { ($0.indices.contains(position.x) ? $0[position.x] : .empty) != .empty }
                : board.lastIndex { ($0.indices.contains(position.x) ? $0[position.x] : .empty) != .empty }
                return Point(x: position.x, y: y!)
            }
        }
    }
    
    func moveForward(steps: Int) {
        for _ in 0 ..< steps {
            let next = nextIndex()
            if boardAt(next) == .ground {
                position = next
            } else {
                return
            }
        }
    }

    func run() -> Int {
        path.forEach { action in
            switch action {
            case .forward(let n):
                moveForward(steps: n)
            case .right:
                facing = facing.next()
            case .left:
                facing = facing.previous()
            }
        }
        return 1000 * (position.y + 1) + 4 * (position.x + 1) + facing.rawValue
    }
}


let map = parseInput(filename: "input")
print(map.run())
