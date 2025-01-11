@preconcurrency import SwiftGodot

@Godot
class Day13Cart: Sprite2D, @unchecked Sendable {
    @Export var verticalRect: Rect2 = .init(position: .zero, size: .zero)
    @Export var horizontalRect: Rect2 = .init(position: .zero, size: .zero)
    @Export var crashRect: Rect2 = .init(position: .zero, size: .zero)
    
    func updateSprite(direction: Vector2i) {
        if direction == .up || direction == .down {
            regionRect = verticalRect
        } else {
            regionRect = horizontalRect
        }
    }
    
    func setCrashed() {
        regionRect = crashRect
    }
}
