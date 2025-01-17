@preconcurrency import SwiftGodot

private enum Tile: Equatable {
    case empty
    case wet
    case water
    case clay
    case sprinkler
    
    var atlasCoords: Vector2i {
        switch self {
        case .empty:
            return Vector2i(x: 16, y: 0)
        case .wet:
            return Vector2i(x: 17, y: 0)
        case .water:
            return Vector2i(x: 16, y: 1)
        case .clay:
            return Vector2i(x: 17, y: 1)
        case .sprinkler:
            return Vector2i(x: 18, y: 0)
        }
    }
}


@Godot
class Day17: Node2D, @unchecked Sendable {
    @SceneTree(path: "TileMapLayer") var tilemap: TileMapLayer?
    @SceneTree(path: "Camera2D") var camera: AocCamera?
    @SceneTree(path: "UI/Label") var resultLabel: Label?
    
    @Export var tickSpeed: Int = 10
    @Export var cameraLerpScale: Double = 0.5
    
    private lazy var scheduler = VisualizationScheduler(
        schedule: .multipleTimesPerPhysicsTicks(n: tickSpeed),
        tick: { [weak self] in self?.tick() },
        render: { [weak self] in self?.render() }
    )
    
    private var boundsRect: Rect2i = Rect2i(position: .zero, size: .zero)
    private var worldRect: Rect2i = Rect2i(position: .zero, size: .zero)
    private var world: [[Tile]] = []
    private var state = State(streams: [])
    private var totalReached = 0
    private var totalStill = 0
    
    private var cameraTopPosition: Vector2 = .zero
    private var cameraTargetPosition: Vector2 = .zero
    
    private func renderWorld() {
        guard let tilemap, let camera else { return }
        
        RenderingServer.setDefaultClearColor(Color(code: "5e5036"))
        cameraTopPosition = camera.position
        cameraTargetPosition = cameraTopPosition
        
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
            let count = worldCountIf { $0 == .water || $0 == .wet }
            resultLabel.text = "Streams: \(state.streams.count), Water: \(count)"
        case .done:
            resultLabel.text = "Result: \(totalReached), \(totalStill)"
        }
    }
    
    private func moveCamera() {
        guard let tilemap, let camera, let cameraParent = camera.getParent() as? Node2D else { return }
        
        let tileSize = Vector2(from: tilemap.tileSet?.tileSize ?? .zero)
        let globalTopPos = cameraParent.toGlobal(localPoint: cameraTopPosition)
        let lowest = getLowestPoint()
        let posOffset = camera.toGlobal(localPoint: Vector2(x: 0, y: Float(lowest) * tileSize.y)) - camera.globalPosition
        
        cameraTargetPosition = cameraTopPosition + posOffset
    }
    
    private func render() {
        moveCamera()
        updateResultLabel()
    }

    override func _ready() {
        setupWorld(deposits: readInput())

        renderWorld()
        render()

        super._ready()
    }
    
    override func _process(delta: Double) {
        if let camera, scheduler.state == .running {
            camera.position = camera.position.lerp(to: cameraTargetPosition, weight: delta * cameraLerpScale)
        }
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

private extension Day17 {
    func readInput() -> [ClayDeposit] {
        let input = FileAccess.getFileAsString(path: "res://input/day17.txt")
        let lines = input.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n")
        let re = /(x|y)=(\d+), (?:x|y)=(\d+)\.\.(\d+)/
        let deposits = lines.map { line in
            let matches = try! re.wholeMatch(in: line)!
            if matches.1 == "x" {
                return ClayDeposit.vertical(x: Int(matches.2)!, y: Int(matches.3)! ... Int(matches.4)!)
            } else {
                return ClayDeposit.horizontal(x: Int(matches.3)! ... Int(matches.4)!, y: Int(matches.2)!)
            }
        }

        return deposits
    }
    
    func setupWorld(deposits: [ClayDeposit]) {
        guard let tilemap else { return }
        let sprinkerPos = Vector2i(x: 500, y: 0)
        
        // Setup tilemap first to have to calculate the bounds for us
        for deposit in deposits {
            switch deposit {
            case .horizontal(let xRange, let y):
                for x in xRange {
                    tilemap.setCell(coords: Vector2i(x: x, y: y), sourceId: 0, atlasCoords: Tile.clay.atlasCoords)
                }
            case .vertical(let x, let yRange):
                for y in yRange {
                    tilemap.setCell(coords: Vector2i(x: x, y: y), sourceId: 0, atlasCoords: Tile.clay.atlasCoords)
                }
            }
        }
        
        
        // This is required to filter which water tiles count for the final answer
        boundsRect = tilemap.getUsedRect()
        boundsRect.size.x += 2
        boundsRect.position.x -= 1
        
        tilemap.setCell(coords: sprinkerPos, sourceId: 0, atlasCoords: Tile.sprinkler.atlasCoords)
        
        // Fill the world from the tilemap cells
        worldRect = tilemap.getUsedRect()
        worldRect.size.x += 2
        worldRect.position.x -= 1
        
        world = [[Tile]](repeating: [Tile](repeating: .empty, count: Int(worldRect.size.x)), count: Int(worldRect.size.y))
        for clayPos in tilemap.getUsedCells() {
            world.set(at: clayPos - worldRect.position, value: .clay)
        }
        world.set(at: sprinkerPos - worldRect.position, value: .sprinkler)
        
        state = State(streams: [[.process(pos: Vector2i(x: 500, y: 0) + .down - worldRect.position)]])
    }
    
    func worldSet(at pos: Vector2i, value: Tile) {
        guard let current = worldGet(at: pos) else { return }
 
        world.set(at: pos, value: value)
        tilemap?.setCell(coords: pos, sourceId: 0, atlasCoords: value.atlasCoords)
    }
    
    func worldGet(at pos: Vector2i) -> Tile? {
        world.safeAt(point: pos)
    }
    
    func worldCountIf(predicate: (Tile) -> Bool) -> Int {
        var total = 0
        world.forCoords { coords in
            if boundsRect.hasPoint(coords + worldRect.position) {
                if predicate(world.at(point: coords)) {
                    total += 1
                }
            }
        }
        return total
    }
    
    func getLowestPoint() -> Int {
        var maxY = 0
        world.forCoords { coords in
            if world.at(point: coords) == .wet || world.at(point: coords) == .water {
                maxY = max(maxY, Int(coords.y))
            }
        }
        return maxY
    }
    
    enum Action: Equatable, Hashable {
        case process(pos: Vector2i)
        case setWet(pos: Vector2i)
        case setWater(pos: Vector2i)
    }
    
    struct State {
        var streams: [[Action]]
    }
    
    func collectAdjacent(to pos: Vector2i, direction: Vector2i) -> (pos: [Vector2i], spils: Bool) {
        var res = [Vector2i]()
        var next = pos + direction
        while let tile = worldGet(at: next), tile == .empty || tile == .wet {
            res.append(next)
            if let below = worldGet(at: next + .down), below == .empty || below == .wet {
                return (pos: res, spils: true)
            }
            next += direction
        }
        return (pos: res, spils: false)
    }
    
    func shouldWaitForWaterToSettle(head: Vector2i) -> Bool {
        let (left, _) = collectAdjacent(to: head, direction: .left)
        let (right, _) = collectAdjacent(to: head, direction: .right)
        
        for p in (left + right) {
            if worldGet(at: p + .down) == .clay {
                return false
            }
        }
        return worldGet(at: head + .down) == .water
    }
    
    func getStreams(byProcessing head: Vector2i) -> [[Action]] {
        guard let current = worldGet(at: head) else { return [] }
        
        // Falling or oob
        if let below = worldGet(at: head + .down) {
            if below == .empty || below == .wet {
                worldSet(at: head, value: .wet)
                return [[.process(pos: head + .down)]]
            }
        } else {
            worldSet(at: head, value: .wet)
            return []
        }
        
        // Determine if this is a container or if it's spilling
        let (left, leftSpils) = collectAdjacent(to: head, direction: .left)
        let (right, rightSpils) = collectAdjacent(to: head, direction: .right)
        
        if leftSpils || rightSpils {
            if shouldWaitForWaterToSettle(head: head) {
                return [[.process(pos: head)]]
            }
            // Fill wet going both sides in parallel, and process below if spilling
            worldSet(at: head, value: .wet)
            var leftQueue = left.map { Action.setWet(pos: $0) }
            var rightQueue = right.map { Action.setWet(pos: $0) }
            if leftSpils {
                leftQueue.append(.process(pos: left.last! + .down))
            }
            if rightSpils {
                rightQueue.append(.process(pos: right.last! + .down))
            }
            return [rightQueue.reversed(), leftQueue.reversed()]
        } else {
            // Fill water going both sides in parallel, and process above when done
            worldSet(at: head, value: .water)
            var leftQueue = left.map { Action.setWater(pos: $0) }
            var rightQueue = right.map { Action.setWater(pos: $0) }
            if let above = worldGet(at: head + .up), above == .empty || above == .wet {
                if leftQueue.count > rightQueue.count {
                    leftQueue.append(.process(pos: head + .up))
                } else {
                    rightQueue.append(.process(pos: head + .up))
                }
            }
            return [leftQueue.reversed(), rightQueue.reversed()]
        }
    }
    
    func dedup(streams: [[Action]]) -> [[Action]] {
        var pending: Set<Action> = []
        var nextStreams: [[Action]] = []
        for stream in streams {
            var nextStream: [Action] = []
            for action in stream {
                if !pending.contains(action) {
                    pending.insert(action)
                    nextStream.append(action)
                }
            }
            if !nextStream.isEmpty {
                nextStreams.append(nextStream)
            }
        }
        return nextStreams
    }
  
    func tick() {
        var nextStreams = [[Action]]()
        
        if state.streams.isEmpty {
            totalReached = worldCountIf { $0 == .water || $0 == .wet }
            totalStill = worldCountIf { $0 == .water }
            GD.print(totalReached)
            GD.print(totalStill)
            scheduler.end()
            return
        }
        
        for var stream in state.streams {
            if let action = stream.popLast() {
                switch action {
                case .process(let pos):
                    nextStreams.append(contentsOf: getStreams(byProcessing: pos))
                case .setWet(let pos):
                    if let tile = worldGet(at: pos), tile == .empty {
                        worldSet(at: pos, value: .wet)
                    }
                case .setWater(let pos):
                    if let tile = worldGet(at: pos), tile == .empty || tile == .wet {
                        worldSet(at: pos, value: .water)
                    }
                }
            }
            
            if !stream.isEmpty {
                nextStreams.append(stream)
            }
        }

        state.streams = dedup(streams: nextStreams)
    }
}
