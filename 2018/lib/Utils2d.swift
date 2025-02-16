@preconcurrency import SwiftGodot

extension Array {
    func at<T>(point: Vector2i) -> T where Element == [T] {
        self[Int(point.y)][Int(point.x)]
    }
    
    func safeAt<T>(point: Vector2i) -> T? where Element == [T] {
        let y = Int(point.y)
        let x = Int(point.x)
        
        if y >= 0 && y < self.count && x >= 0 && x < self[y].count {
            return self[y][x]
        }
        return nil
    }
    
    mutating func set<T>(at point: Vector2i, value: T) where Element == [T] {
        self[Int(point.y)][Int(point.x)] = value
    }
    
    func forCoords<T>(block: (Vector2i) -> Void) where Element == [T] {
        for y in 0 ..< self.count {
            for x in 0 ..< self[y].count {
                block(Vector2i(x: Int32(x), y: Int32(y)))
            }
        }
    }
    
    func mapCoords<T>(block: (Vector2i) -> T) -> [[T]] where Element == [T] {
        var result = [[T]]()
        for y in 0 ..< self.count {
            var line = [T]()
            for x in 0 ..< self[y].count {
                line.append(block(Vector2i(x: Int32(x), y: Int32(y))))
            }
            result.append(line)
        }
        return result
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

extension Vector2i {
    init(x: Int, y: Int) {
        self.init(x: Int32(x), y: Int32(y))
    }
    
    func turn(offset: Int) -> Vector2i {
        let directions = Vector2i.directions()
        let current = directions.firstIndex(of: self)!
        let next = (current + offset + directions.count) % directions.count
        return directions[next]
    }
    
    static func directions() -> [Vector2i] {
        [.up, .right, .down, .left]
    }
    
    static func directionsAndDiagonals() -> [Vector2i] {
        [.up, .right, .down, .left, (.up + .left), (.up + .right), (.down + .left), (.down + .right)]
    }
}
