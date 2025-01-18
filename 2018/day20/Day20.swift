@preconcurrency import SwiftGodot

private enum Tile: Equatable {
    case room
    case wall
    case door
    case unknown
    
    case player
    case origin
    
    func atlasCoords(pos: Vector2i) -> Vector2i {
        switch self {
        case .room:
            return Vector2i(x: pos.randomStableTileOffset(range: 0...3), y: 10)
        case .wall:
            return Vector2i(x: pos.randomStableTileOffset(range: 0...1), y: 9)
        case .door:
            return Vector2i(x: pos.randomStableTileOffset(range: 2...3), y: 9)
        case .unknown:
            return Vector2i(x: 4, y: 10)
        case .player:
            return Vector2i(x: 4, y: 9)
        case .origin:
            return Vector2i(x: 5, y: 9)
        }
    }
}

private enum PathRegex: Character, Equatable {
    case north = "N"
    case east = "E"
    case south = "S"
    case west = "W"
    case openBranch = "("
    case closeBranch = ")"
    case nextPath = "|"
    
    var directionOffset: Vector2i {
        switch self {
        case .north:
            return .up
        case .east:
            return .right
        case .south:
            return .down
        case .west:
            return .left
        case .openBranch, .closeBranch, .nextPath:
            return .zero
        }
    }
}

private struct CurrentPath: Equatable, Hashable {
    var index: Int
    var position: Vector2i
    var branchJumpStack: [Int]
}

private enum State {
    case generating
    case exploring
}

@Godot
class Day20: Node2D, @unchecked Sendable {
    @SceneTree(path: "TileMapLayer") var tilemap: TileMapLayer?
    @SceneTree(path: "Camera2D") var camera: AocCamera?
    @SceneTree(path: "UI/Label") var resultLabel: Label?
    
    @Export var tickSpeedIsMultiplicative: Bool = false
    @Export var tickSpeed: Int = 10
    
    private lazy var scheduler = VisualizationScheduler(
        schedule: {
            tickSpeedIsMultiplicative ? .multipleTimesPerPhysicsTicks(n: tickSpeed) : .everyNPhysicsTicks(n: tickSpeed)
        }(),
        tick: { [weak self] in self?.tick() },
        render: { [weak self] in self?.render() }
    )
    
    private var state: State = .generating
    private var world: [Vector2i: Tile] = [:]
    
    private var pathRegex: [PathRegex] = []
    private var processQueue: [CurrentPath] = []
    
    private var pathMemo: Set<CurrentPath> = []
    private var branchMemo: [Int: Branch] = [:]
    
    private var dfsMemo: [Set<Vector2i>: Int] = [:]
    private var longestPath = -1
    private var farRooms = -1
    
    private func renderWorld(clear: Bool) {
        guard let tilemap else { return }
        
        if clear {
            tilemap.clear()
        }
        
        let player = processQueue.last?.position
        for (coords, tile) in world {
            let displayTile: Tile = {
                if coords == player {
                    return .player
                } else if coords == .zero {
                    return .origin
                }
                return tile
            }()
            tilemap.setCell(coords: coords, sourceId: 0, atlasCoords: displayTile.atlasCoords(pos: coords))
        }
    }

    private func updateResultLabel() {
        guard let resultLabel else { return }
        
        switch scheduler.state {
        case .loading:
            resultLabel.text = ""
        case .running, .paused:
            resultLabel.text = "World size: \(world.count)"
        case .done:
            resultLabel.text = "Part 1: \(longestPath)\nPart 2: \(farRooms)"
        }
    }
    
    private func render() {
        renderWorld(clear: false)
        updateResultLabel()
    }

    override func _ready() {
        pathRegex = readInput()
        setupWorld()

        renderWorld(clear: true)
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

private struct Branch {
    var starts: [Int]
    var end: Int
}

private extension Day20 {
    func readInput() -> [PathRegex] {
        return FileAccess.getFileAsString(path: "res://input/day20.txt")
            .trimmingCharacters(in: .whitespacesAndNewlines)
            .compactMap(PathRegex.init(rawValue:))
    }
    
    func setupWorld() {
        // Always start in a room at (0, 0)
        discoverRoom(at: .zero, door: nil)
        processQueue = [CurrentPath(index: 0, position: .zero, branchJumpStack: [])]
    }
    
    func finalizeWorld() {
        world = world.mapValues({ tile in
            tile == .unknown ? .wall : tile
        })
    }

    func discoverRoom(at position: Vector2i, door: Vector2i?) {
        if !world.keys.contains(position) {
            world[position] = .room
            for pos in Vector2i.directionsAndDiagonals() {
                let target = position + pos
                if pos.x == 0 || pos.y == 0 {
                    if !world.keys.contains(target) {
                        world[target] = .unknown
                    }
                } else {
                    world[target] = .wall
                }
            }
        }
        if let door {
            world[door] = .door
        }
    }
    
    func move(path: inout CurrentPath, direction: Vector2i) {
        discoverRoom(at: path.position + direction * 2, door: path.position + direction)
        path.position += direction * 2
    }
    
    func getOrCreateBranch(for index: Int) -> Branch {
        if let branch = branchMemo[index] {
            return branch
        }
        
        var subBranch = 0
        var starts: [Int] = [index + 1]
        var hasEmptyOption = false

        var i = index + 1
        while i < pathRegex.count && (subBranch > 0 || pathRegex[i] != .closeBranch) {
            if pathRegex[i] == .openBranch {
                subBranch += 1
            } else if pathRegex[i] == .closeBranch {
                subBranch -= 1
            } else if subBranch == 0 {
                if pathRegex[i] == .nextPath {
                    if (i + 1) < pathRegex.count && pathRegex[i + 1] == .nextPath {
                        hasEmptyOption = true
                        i += 1
                    } else if (i + 1) < pathRegex.count && pathRegex[i + 1] == .closeBranch {
                        hasEmptyOption = true
                    } else {
                        starts.append(i + 1)
                    }
                }
            }
            i += 1
        }
        
        if hasEmptyOption {
            starts.append(i + 1)
        }
        
        let branch = Branch(starts: starts, end: i + 1)
        branchMemo[index] = branch
        return branch
    }
    
    func split(path: CurrentPath) -> [CurrentPath] {
        let branch = getOrCreateBranch(for: path.index)
        let ret = branch.starts.map { branchStart in
            var nextStack = path.branchJumpStack
            if branch.end != branchStart {
                nextStack.append(branch.end)
            }
            return CurrentPath(
                index: branchStart,
                position: path.position,
                branchJumpStack: nextStack
            )
        }
        return ret
    }
    
    func tick() {
        switch state {
        case .generating:
            tickGenerate()
        case .exploring:
            tickExplore()
        }
    }
    
    func availableNeighbors(for position: Vector2i, alreadyVisited: Set<Vector2i>) -> [Vector2i] {
        Vector2i.directions()
            .filter { world[position + $0] == .door }
            .map { position + $0 * 2 }
            .filter { !alreadyVisited.contains($0) }
    }
    
    func dfs(current: Vector2i, path: Set<Vector2i> = []) -> Int {
        var path = path
        path.insert(current)

        if let result = dfsMemo[path] {
            return result
        }
        
        let neighbors = availableNeighbors(for: current, alreadyVisited: path)

        let result: Int = {
            switch neighbors.count {
            case 0:
                return path.count - 1
            case 1:
                return dfs(current: neighbors.first!, path: path)
            default:
                return neighbors.map { dfs(current: $0, path: path) }.max()!
            }
        }()
        dfsMemo[path] = result
        return result
    }
    
    func bfs(start: Vector2i) -> Set<Vector2i> {
        var visited: Set<Vector2i> = []
        var valid: Set<Vector2i> = []
        
        var steps = 0
        var queue = [start]
        while !queue.isEmpty {
            var nextQueue = [Vector2i]()
            for item in queue {
                if visited.contains(item) {
                    continue
                }
                
                visited.insert(item)
                if steps >= 1000 {
                    valid.insert(item)
                }
                
                for other in availableNeighbors(for: item, alreadyVisited: visited) {
                    nextQueue.append(other)
                }
            }
            queue = nextQueue
            steps += 1
        }
        return valid
    }
    
    func tickExplore() {
        // Too lazy to make it interactive/visualizable
        longestPath = dfs(current: .zero)
        GD.print("Longest path: \(longestPath)")
        farRooms = bfs(start: .zero).count
        GD.print("Rooms at least 1000 away: \(farRooms)")
        scheduler.end()
    }

    func tickGenerate() {
        guard !processQueue.isEmpty else {
            finalizeWorld()
            state = .exploring
            return
        }
        
        let lastIndex = processQueue.count - 1
        let current = processQueue[lastIndex]
        guard current.index < pathRegex.count else {
            processQueue.popLast()
            tickGenerate()
            return
        }
        
        guard !pathMemo.contains(current) else {
            processQueue.popLast()
            tickGenerate()
            return
        }
        
        pathMemo.insert(current)
        
        let item = pathRegex[current.index]
        switch item {
        case .north, .east, .west, .south:
            discoverRoom(at: current.position + item.directionOffset * 2, door: current.position + item.directionOffset)
            processQueue[lastIndex].position += item.directionOffset * 2
            processQueue[lastIndex].index += 1
        case .openBranch:
            processQueue.append(contentsOf: split(path: processQueue.popLast()!))
            tickGenerate()
        case .nextPath:
            if let nextIndex = processQueue[lastIndex].branchJumpStack.popLast() {
                processQueue[lastIndex].index = nextIndex
                tickGenerate()
            } else {
                GD.pushError("Invalid | without a jump index")
                scheduler.end()
            }
        case .closeBranch:
            processQueue[lastIndex].index += 1
            tickGenerate()
        }
    }
}

private extension Vector2i {
    func randomStableTileOffset(range: ClosedRange<Int>) -> Int {
        return (((self.hashValue % range.count) + range.count) % range.count) + range.lowerBound
    }
}
