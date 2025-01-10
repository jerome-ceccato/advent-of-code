@preconcurrency import SwiftGodot

extension [[Int]] {
    func at(point: Vector2i) -> Int {
        self[Int(point.y)][Int(point.x)]
    }
    
    mutating func set(at point: Vector2i, value: Int) {
        self[Int(point.y)][Int(point.x)] = value
    }
}

extension Rect2i {
    func foreach(block: (Vector2i) -> Void) {
        for y in position.y ..< end.y {
            for x in position.x ..< end.x {
                block(Vector2i(x: x, y: y))
            }
        }
    }
}
