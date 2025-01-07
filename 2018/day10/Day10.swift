@preconcurrency import SwiftGodot

struct Light {
    var position: Vector2i
    var velocity: Vector2i
}

enum Tile {
    case light
    case done
    
    var atlastCoords: Vector2i {
        switch self {
        case .light:
            return Vector2i(x: 0, y: 0)
        case .done:
            return Vector2i(x: 1, y: 0)
        }
    }
}

@Godot
class Day10: Node2D, @unchecked Sendable {
    @Export(.nodeType, "TileMapLayer") var tilemap: TileMapLayer?
    @Export(.nodeType, "Camera2D") var camera: Camera2D?
    @Export(.nodeType, "Label") var tickCountLabel: Label?
    
    private lazy var scheduler = VisualizationScheduler(
        schedule: .everyNPhysicsTicks(n: 3),
        tick: { [weak self] in
            self?.tick()
        }
    )
    private var lights: [Light] = []
    private var speed = 200
    private var tickCount = 0
    private var hasStarted = false
    
    private func updateTileMap() {
        guard let tilemap else { return }

        tilemap.clear()
        for light in lights {
            let tile: Tile = scheduler.done ? .done : .light
            tilemap.setCell(coords: light.position, sourceId: 0, atlasCoords: tile.atlastCoords)
        }
    }
    
    private func updateTickLabel() {
        guard let tickCountLabel else { return }
        
        if hasStarted {
            tickCountLabel.text = "Time elapsed: \(tickCount)"
        } else {
            tickCountLabel.text = "Fast forwarded \(tickCount) seconds"
        }
    }
    
    override func _ready() {
        lights = readInput()
        fastForward()
        
        updateTileMap()
        updateTickLabel()
        centerCamera()

        super._ready()
    }
    
    override func _physicsProcess(delta: Double) {
        scheduler.onPhysicsProcess(delta: delta)
    }
    
    override func _unhandledInput(event: InputEvent?) {
        scheduler.onInput(event: event)
    }
}

private extension Day10 {
    func readInput() -> [Light] {
        let input = FileAccess.getFileAsString(path: "res://input/day10.txt")
        let re = /position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+), *(-?\d+)>/
        
        return input.components(separatedBy: "\n").flatMap { line in
            guard let match = try? re.wholeMatch(in: line) else {
                return nil
            }
            return Light(
                position: Vector2i(x: Int32(match.1)!, y: Int32(match.2)!),
                velocity: Vector2i(x: Int32(match.3)!, y: Int32(match.4)!)
            )
        }
    }
    
    func centerCamera() {
        guard let tilemap, let camera else { return }
        
        let bounds = tilemap.getUsedRect()
        let topLeft = tilemap.mapToLocal(mapPosition: bounds.position)
        let bottomRight = tilemap.mapToLocal(mapPosition: bounds.end)
        let center = topLeft + (bottomRight - topLeft) / 2
        camera.position = center
    }
    
    func getBounds(lights: [Light]) -> Rect2i {
        var bounds = Rect2i(position: lights.first!.position, size: Vector2i.zero)
        for light in lights {
            bounds.position.x = min(bounds.position.x, light.position.x)
            bounds.position.y = min(bounds.position.y, light.position.y)
            bounds.end.x = max(bounds.position.x, light.position.x)
            bounds.end.y = max(bounds.position.y, light.position.y)
        }
        return bounds
    }
    
    func moveLights(lights: [Light], speed: Int) -> [Light] {
        lights.map { light in
            Light(position: light.position + (light.velocity * speed), velocity: light.velocity)
        }
    }
    
    func fastForward() {
        while speed > 1 {
            let currentArea = getBounds(lights: lights)
            let nextLightsPosition = moveLights(lights: lights, speed: speed)
            let nextArea = getBounds(lights: nextLightsPosition)
            
            if nextArea.size.x > currentArea.size.x || nextArea.size.y > currentArea.size.y {
                if speed > 100 {
                    tickCount -= speed
                    lights = moveLights(lights: lights, speed: -speed)
                    speed = 50
                } else if speed > 1 {
                    tickCount -= speed
                    lights = moveLights(lights: lights, speed: -speed)
                    speed = 1
                }
            } else {
                tickCount += speed
                lights = nextLightsPosition
            }
        }
    }
    
    func tick() {
        let currentArea = getBounds(lights: lights)
        let nextLightsPosition = moveLights(lights: lights, speed: speed)
        let nextArea = getBounds(lights: nextLightsPosition)
        
        if nextArea.size.x > currentArea.size.x || nextArea.size.y > currentArea.size.y {
            scheduler.done = true
            zoomOnAnswer()
        } else {
            tickCount += speed
            lights = nextLightsPosition
        }
        
        hasStarted = true
        updateTileMap()
        updateTickLabel()
    }
    
    func zoomOnAnswer() {
        getTree()?
            .createTween()?
            .tweenProperty(object: camera, property: "zoom", finalVal: Variant(Vector2(x: 1.5, y: 1.5)), duration: 1)?
            .setDelay(0.5)?
            .setEase(.inOut)
    }
}
