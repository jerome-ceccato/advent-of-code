@preconcurrency import SwiftGodot

@Godot
class AocCamera: Camera2D, @unchecked Sendable {
    let zoomMin = Vector2(x: 0.1, y: 0.1)
    let zoomMax = Vector2(x: 10, y: 10)
    
    private var dragOriginalPos: Vector2 = .zero
    private var mouseStartPos: Vector2 = .zero
    private var isDragging = false
    
    func center(on tilemap: TileMapLayer) {
        let bounds = tilemap.getUsedRect()
        let topLeft = tilemap.mapToLocal(mapPosition: bounds.position)
        let bottomRight = tilemap.mapToLocal(mapPosition: bounds.end)
        let center = topLeft + (bottomRight - topLeft) / 2
        
        position = center
    }
    
    private func apply(zoom newZoom: Vector2) {
        if newZoom.x < zoomMin.x || newZoom.y < zoomMin.y {
            zoom = zoomMin
        } else if newZoom.x > zoomMax.x || newZoom.y > zoomMax.y {
            zoom = zoomMax
        } else {
            zoom = newZoom
        }
    }
    
    override func _unhandledInput(event: InputEvent?) {
        guard let event else { return }
        
        if event.isActionPressed(action: "zoom_in") {
            apply(zoom: zoom / 1.5)
        } else if event.isActionPressed(action: "zoom_out") {
            apply(zoom: zoom * 1.5)
        } else if let event = event as? InputEventMouse, event.isAction("drag") {
            if event.isPressed() {
                mouseStartPos = event.position
                dragOriginalPos = position
                isDragging = true
            } else {
                isDragging = false
            }
        } else if let event = event as? InputEventMouseMotion, isDragging {
            position = Vector2(x: 1.0 / zoom.x, y: 1.0 / zoom.y) * (mouseStartPos - event.position) + dragOriginalPos
//            GD.print(position)
        }
    }
}
