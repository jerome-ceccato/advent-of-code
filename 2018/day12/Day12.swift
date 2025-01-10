@preconcurrency import SwiftGodot

private enum Tile: Equatable {
    case empty
    case full
    case growing
    case dying
    
    init(previous: Bool, next: Bool) {
        switch (previous, next) {
        case (true, true):
            self = .full
        case (false, false):
            self = .empty
        case (false, true):
            self = .growing
        case (true, false):
            self = .dying
        }
    }
    
    var atlasCoords: Vector2i {
        switch self {
        case .empty:
            return Vector2i(x: 0, y: 1)
        case .full:
            return Vector2i(x: 1, y: 1)
        case .growing:
            return Vector2i(x: 2, y: 1)
        case .dying:
            return Vector2i(x: 3, y: 1)
        }
    }
}

@Godot
class Day12: Node2D, @unchecked Sendable {
    @SceneTree(path: "TileMapLayer") var tilemap: TileMapLayer?
    @SceneTree(path: "Camera2D") var camera: AocCamera?
    @SceneTree(path: "UI/Label") var resultLabel: Label?
    
    @Export var generationTarget: Int = 20
    
    private lazy var scheduler = VisualizationScheduler(
        schedule: .everyNPhysicsTicks(n: 2),
        tick: { [weak self] in self?.tick() },
        render: { [weak self] in self?.renderAll() }
    )
    
    private var generation = 0
    private var board: Set<Int> = []
    private var rules: Set<[Bool]> = []
    
    private var tiles: [Tile] = []
    private var isTransitionTick = false
    
    private var history: [[Bool]: Int] = [:]
    private var isStable = false
    private var cyclePeriod = 0
    private var cycleOffset = 0
    
    private func updateTileMap() {
        guard let tilemap else { return }

        tilemap.clear()
        for (x, tile) in tiles.enumerated() {
            tilemap.setCell(coords: Vector2i(x: Int32(x), y: 0), sourceId: 0, atlasCoords: tile.atlasCoords)
        }
    }
    
    private func updateResultLabel() {
        guard let resultLabel else { return }
        
        let score = board.reduce(0, +)
        resultLabel.text = "Generation \(generation): \(score)"
    }
    
    private func renderAll() {
        updateTileMap()
        updateResultLabel()
        
        if let tilemap, let camera {
            camera.center(on: tilemap)
        }
    }
    
    private var zoomStarted = false
    private func zoomCameraSmooth() {
        guard !zoomStarted else { return }
        zoomStarted = true
        
        getTree()?
            .createTween()?
            .tweenProperty(object: camera, property: "zoom", finalVal: Variant(Vector2(x: 1, y: 1)), duration: 15)?
            .setEase(.inOut)
    }
    
    override func _ready() {
        readInput()
        tiles = buildTiles()
        renderAll()
        
        super._ready()
    }
    
    override func _physicsProcess(delta: Double) {
        scheduler.onPhysicsProcess(delta: delta)
    }
    
    override func _unhandledInput(event: InputEvent?) {
        scheduler.onInput(event: event, node: self)
    }
}

private extension Day12 {
    func readInput() {
        let input = FileAccess.getFileAsString(path: "res://input/day12.txt")
        let parts = input.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n\n")
        
        board = parts[0].components(separatedBy: ": ").last!.enumerated().reduce(into: Set<Int>(), { partialResult, item in
            if item.element == "#" {
                partialResult.insert(item.offset)
            }
        })

        rules = parts[1].components(separatedBy: "\n").reduce(into: Set<[Bool]>(), { partialResult, line in
            let parts = line.components(separatedBy: " => ")
            if parts[1] == "#" {
                partialResult.insert(parts[0].map { $0 == "#" })
            }
        })
    }
    
    func buildTiles() -> [Tile] {
        return bounds(board: board, offset: 1).map { x in
            board.contains(x) ? .full : .empty
        }
    }
    
    func buildTileDiff(prev: Set<Int>, next: Set<Int>) -> [Tile] {
        let low = min(prev.min()!, next.min()!) - 1
        let high = max(prev.max()!, next.max()!) + 1
        
        return (low ... high).map { x in
            Tile(previous: prev.contains(x), next: next.contains(x))
        }
    }
    
    func next(at index: Int) -> Int? {
        let pattern = [
            board.contains(index - 2),
            board.contains(index - 1),
            board.contains(index),
            board.contains(index + 1),
            board.contains(index + 2),
        ]
        return rules.contains(pattern) ? index : nil
    }
    
    func bounds(board: Set<Int>, offset: Int) -> ClosedRange<Int> {
        let min = board.min()!
        let max = board.max()!
        
        return (min - offset) ... (max + offset)
    }
    
    func updateTransitionState() -> Bool {
        let transitioning = tiles.contains { tile in
            tile == .dying || tile == .growing
        }
        
        if transitioning {
            tiles = tiles.map { tile in
                switch tile {
                case .empty, .dying:
                    return .empty
                case .full, .growing:
                    return .full
                }
            }
        }
        return transitioning
    }
    
    func flatRepresentation(of board: Set<Int>) -> [Bool] {
        bounds(board: board, offset: 0).map { x in
            board.contains(x)
        }
    }
    
    func getFastForwardSteps() -> Int {
        // taken randomly to make the counter look like it increases extremely fast
        let lookAhead = 451_583_923
        
        if (generation + lookAhead * cyclePeriod) < generationTarget {
            return lookAhead * cyclePeriod
        } else {
            return (generationTarget - generation) / cyclePeriod - 1
        }
    }
  
    func tick() {
        guard generation < generationTarget else {
            scheduler.end()
            GD.print(board.reduce(0, +))
            return
        }
        
        zoomCameraSmooth()
        if isTransitionTick {
            isTransitionTick = updateTransitionState()
        } else {
            if isStable && (generationTarget - generation) > 10 {
                let steps = getFastForwardSteps()
                board = Set(board.map { $0 + steps * cycleOffset })
                generation += steps
            } else {
                let oldBoard = board
                board = Set(bounds(board: board, offset: 2).compactMap(next(at:)))
                generation += 1
                let representation = flatRepresentation(of: board)
                
                
                if let previousGen = history[representation] {
                    isStable = true
                    cyclePeriod = generation - previousGen
                    cycleOffset = board.min()! - oldBoard.min()!
                } else {
                    history[representation] = generation
                }
                
                isTransitionTick = true
                tiles = buildTileDiff(prev: oldBoard, next: board)
            }
        }
    }
}
