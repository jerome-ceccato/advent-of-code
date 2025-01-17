@preconcurrency import SwiftGodot

private enum Tile: Equatable {
    case open
    case tree
    case lumberyard
    
    init?(raw: Character) {
        switch raw {
        case ".":
            self = .open
        case "|":
            self = .tree
        case "#":
            self = .lumberyard
        default:
            return nil
        }
    }
    
    var atlasCoords: Vector2i {
        switch self {
        case .open:
            return Vector2i(x: 6, y: 12)
        case .tree:
            return Vector2i(x: 7, y: 12)
        case .lumberyard:
            return Vector2i(x: 6, y: 13)
        }
    }
}


@Godot
class Day18: Node2D, @unchecked Sendable {
    @SceneTree(path: "TileMapLayer") var tilemap: TileMapLayer?
    @SceneTree(path: "Camera2D") var camera: AocCamera?
    @SceneTree(path: "UI/Label") var resultLabel: Label?
    
    @Export var tickSpeedIsMultiplicative: Bool = false
    @Export var tickSpeed: Int = 10
    @Export var targetMinutes: Int = 1000000000
    
    private lazy var scheduler = VisualizationScheduler(
        schedule: {
            tickSpeedIsMultiplicative ? .multipleTimesPerPhysicsTicks(n: tickSpeed) : .everyNPhysicsTicks(n: tickSpeed)
        }(),
        tick: { [weak self] in self?.tick() },
        render: { [weak self] in self?.render() }
    )
    
    private var world: [[Tile]] = []
    private var minutesElapsed = 0
    private var previousState: [[[Tile]]: Int] = [:]
    
    private var totalTrees = 0
    private var totalLumberyards = 0
    
    private func renderWorld() {
        guard let tilemap else { return }
        
        tilemap.clear()
        world.forCoords { coords in
            tilemap.setCell(coords: coords, sourceId: 0, atlasCoords: world.at(point: coords).atlasCoords)
        }
    }
   
    private func updateResultLabel() {
        guard let resultLabel else { return }
        
        switch scheduler.state {
        case .loading:
            resultLabel.text = ""
        case .running, .paused:
            resultLabel.text = "Minutes: \(minutesElapsed)"
        case .done:
            resultLabel.text = "Total: \(totalTrees * totalLumberyards) (\(totalTrees) * \(totalLumberyards))"
        }
    }
    
    private func render() {
        renderWorld()
        updateResultLabel()
    }

    override func _ready() {
        world = readInput()

        render()
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

private enum ClayDeposit {
    case horizontal(x: ClosedRange<Int>, y: Int)
    case vertical(x: Int, y: ClosedRange<Int>)
}

private extension Day18 {
    func readInput() -> [[Tile]] {
        return FileAccess.getFileAsString(path: "res://input/day18.txt")
            .trimmingCharacters(in: .whitespacesAndNewlines)
            .components(separatedBy: "\n")
            .map { line in line.compactMap(Tile.init) }
    }
    
    func worldCount(tile: Tile) -> Int {
        world.reduce(0) { partialResult, line in
            partialResult + line.count { $0 == tile }
        }
    }
    
    func neighboringTiles(for position: Vector2i) -> [Tile] {
        Vector2i.directionsAndDiagonals().compactMap { world.safeAt(point: $0 + position) }
    }
    
    func countNeighbors(of position: Vector2i, withType target: Tile) -> Int {
        neighboringTiles(for: position).filter { $0 == target }.count
    }
    
    func nextTile(for position: Vector2i) -> Tile {
        switch world.at(point: position) {
        case .open:
            if countNeighbors(of: position, withType: .tree) >= 3 {
                return .tree
            } else {
                return .open
            }
        case .tree:
            if countNeighbors(of: position, withType: .lumberyard) >= 3 {
                return .lumberyard
            } else {
                return .tree
            }
        case .lumberyard:
            if countNeighbors(of: position, withType: .lumberyard) >= 1 && countNeighbors(of: position, withType: .tree) >= 1 {
                return .lumberyard
            } else {
                return .open
            }
        }
    }
  
    func tick() {
        guard scheduler.state == .running else { return }
        
        world = world.mapCoords(block: nextTile(for:))
        minutesElapsed += 1
        
        if targetMinutes - minutesElapsed > 200 {
            if let cycle = previousState[world] {
                let period = minutesElapsed - cycle
                let skipAheads: Int = ((targetMinutes - minutesElapsed) / period) - (100 / period)
                minutesElapsed += (period * skipAheads)
            } else {
                previousState[world] = minutesElapsed
            }
        }
        
        if minutesElapsed >= targetMinutes {
            totalTrees = worldCount(tile: .tree)
            totalLumberyards = worldCount(tile: .lumberyard)
            GD.print(totalTrees * totalLumberyards)
            scheduler.end()
        }
    }
}
