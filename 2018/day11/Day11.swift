@preconcurrency import SwiftGodot

private enum Tile {
    case best
    case power(level: Int)
    
    var atlasCoords: Vector2i {
        switch self {
        case .best:
            return Vector2i(x: 2, y: 0)
        case .power(let level):
            return Vector2i(x: Int32(level), y: 1)
        }
    }
}

@Godot
class Day11: Node2D, @unchecked Sendable {
    @SceneTree(path: "TileMapLayer") var tilemap: TileMapLayer?
    @SceneTree(path: "Cursor") var cursor: NinePatchRect?
    @SceneTree(path: "Camera2D") var camera: AocCamera?
    @SceneTree(path: "UI/Label") var resultLabel: Label?
    
    @Export var ticksPerPhysicsTicks: Int = 128
    
    private lazy var scheduler = VisualizationScheduler(
        schedule: .multipleTimesPerPhysicsTicks(n: ticksPerPhysicsTicks),
        tick: { [weak self] in self?.tick() },
        render: { [weak self] in self?.renderAll(reload: false) }
    )
    
    private let gridRect = Rect2i(position: .zero, size: Vector2i(x: 300, y: 300))
    private var serialNumber = -1
    private var grid: [[Int]]  = []
    
    private var currentRect = Rect2i(position: .zero, size: Vector2i(x: 3, y: 3))
    private var bestRect = Rect2i(position: .zero, size: .zero)
    private var bestScore = -100
    
    private var oldBestRect = Rect2i(position: .zero, size: .zero)
    
    private func getTile(at point: Vector2i) -> Tile {
        if bestRect.hasPoint(point) {
            return .best
        }
        return .power(level: grid.at(point: point) + 5)
    }
    
    private func rerender(rect: Rect2i) {
        guard let tilemap else { return }
        
        rect.foreach { coords in
            let tile = getTile(at: coords)
            tilemap.setCell(
                coords: coords,
                sourceId: 0,
                atlasCoords: getTile(at: coords).atlasCoords
            )
        }
    }
    
    private func updateTileMap(reload: Bool) {
        guard let tilemap else { return }

        if reload {
            tilemap.clear()
            rerender(rect: gridRect)
        } else {
            if bestRect != oldBestRect {
                rerender(rect: oldBestRect)
                rerender(rect: bestRect)
            }
            oldBestRect = bestRect
        }
    }
    
    private func updateCursor() {
        guard let cursor, let tilemap, scheduler.hasStarted else { return }
        
        let pos = tilemap.mapToLocal(mapPosition: currentRect.position)
        let end = tilemap.mapToLocal(mapPosition: currentRect.end)
        let size = end - pos
        cursor.setPosition(pos - Vector2(x: 4, y: 4))
        cursor.setSize(size)
    }
    
    private func updateResultLabel() {
        guard let resultLabel else { return }
        
        if scheduler.hasStarted {
            resultLabel.text = "\(bestRect.position.x),\(bestRect.position.y),\(bestRect.size.y) (\(bestScore))"
        } else {
            resultLabel.text = "Serial number: \(serialNumber)"
        }
    }
    
    private func renderAll(reload: Bool) {
        updateTileMap(reload: reload)
        updateCursor()
        updateResultLabel()
    }
    
    override func _ready() {
        serialNumber = readInput()
        setupGrid()
        
        renderAll(reload: true)
        if let tilemap, let camera {
            camera.center(on: tilemap)
        }

        super._ready()
    }
    
    override func _physicsProcess(delta: Double) {
        scheduler.onPhysicsProcess(delta: delta)
    }
    
    override func _unhandledInput(event: InputEvent?) {
        scheduler.onInput(event: event, node: self)
    }
}

private extension Day11 {
    func readInput() -> Int {
        let input = FileAccess.getFileAsString(path: "res://input/day11.txt")
        return Int(input.trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0
    }
    
    func gridPowerLevel(at point: Vector2i) -> Int {
        let rackId = Int(point.x) + 10
        var powerLevel = rackId * Int(point.y)
        powerLevel += serialNumber
        powerLevel *= rackId
        powerLevel = (powerLevel % 1000) / 100
        return powerLevel - 5
    }
    
    func setupGrid() {
        grid = [[Int]](repeating: [Int](repeating: 0, count: Int(gridRect.size.x)), count: Int(gridRect.size.y))
        gridRect.foreach { coords in
            grid.set(at: coords, value: gridPowerLevel(at: coords))
        }
    }
    
    func getScore(rect: Rect2i) -> Int {
        var score = 0
        rect.foreach { coords in
            score += grid.at(point: coords)
        }
        return score
    }
    
    func getNextRect(from rect: Rect2i) -> Rect2i? {
        let possibilities: [Rect2i] = [
            Rect2i(position: rect.position, size: rect.size + .one),
            Rect2i(position: rect.position + .right, size: .one),
            Rect2i(position: Vector2i(x: 0, y: rect.position.y + 1), size: .one)
        ]
        
        for next in possibilities {
            // Remove for the real result, this helps the display not lag too much
            if next.size.y < 20 {
                if gridRect.encloses(b: next) {
                    return next
                }
            }
        }
        return nil
    }
  
    func tick() {
        let score = getScore(rect: currentRect)
        if score > bestScore {
            bestScore = score
            bestRect = currentRect
        }
        
        if let next = getNextRect(from: currentRect) {
            currentRect = next
        } else {
            scheduler.done = true
        }

        renderAll(reload: false)
    }
}

private extension [[Int]] {
    func at(point: Vector2i) -> Int {
        self[Int(point.y)][Int(point.x)]
    }
    
    mutating func set(at point: Vector2i, value: Int) {
        self[Int(point.y)][Int(point.x)] = value
    }
}

private extension Rect2i {
    func foreach(block: (Vector2i) -> Void) {
        for y in position.y ..< end.y {
            for x in position.x ..< end.x {
                block(Vector2i(x: x, y: y))
            }
        }
    }
}
