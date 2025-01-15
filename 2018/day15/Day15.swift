@preconcurrency import SwiftGodot

/* Assets from:
 * https://deepdivegamestudio.itch.io/humanoid-asset-pack
 * https://deepdivegamestudio.itch.io/humanoid2-asset-pack
 * https://askariot.itch.io/game-tileset
 */
private enum Tile: Equatable {
    case empty
    case wall
    case deadBody
    case elf
    case goblin
    
    private static func randomTileId(for position: Vector2i) -> Int {
        abs((position.x + position.y * 1000).hashValue) % 4
    }
    
    func atlasCoords(for position: Vector2i) -> Vector2i {
        switch self {
        case .empty:
            return Vector2i(x: Tile.randomTileId(for: position), y: 4)
        case .wall:
            return Vector2i(x: Tile.randomTileId(for: position), y: 5)
        case .deadBody:
            return Vector2i(x: 4, y: 4)
        case .goblin:
            return Vector2i(x: 1, y: 6)
        case .elf:
            return Vector2i(x: 0, y: 6)
        }
    }
}

private final class Unit {
    let kind: Kind
    var position: Vector2i
    let attackPower: Int
    var health: Int
    
    enum Kind: Equatable {
        case elf
        case goblin
        
        var tile: Tile {
            switch self {
            case .elf:
                return .elf
            case .goblin:
                return .goblin
            }
        }
    }
    
    init(kind: Kind, position: Vector2i, attackPower: Int = 3) {
        self.kind = kind
        self.position = position
        self.attackPower = attackPower
        self.health = 200
    }
}

private struct State {
    var round: Int = 0

    var currentAttackPower: Int = 3
    var elfAttackPowerLow: Int = 3
    var elfAttackPowerHigh: Int = 200
}

@Godot
class Day15: Node2D, @unchecked Sendable {
    @SceneTree(path: "World") var worldTilemap: TileMapLayer?
    @SceneTree(path: "Units") var unitsTilemap: TileMapLayer?
    @SceneTree(path: "Camera2D") var camera: AocCamera?
    @SceneTree(path: "UI/Label") var resultLabel: Label?
    
    @Export var tickSpeed: Int = 10
    
    private lazy var scheduler = VisualizationScheduler(
        schedule: .everyNPhysicsTicks(n: tickSpeed),
        tick: { [weak self] in self?.tick() },
        render: { [weak self] in self?.render() }
    )
    
    private var world: [[Tile]] = []
    private var units: [Unit] = []
    private var state = State()
    
    private func renderWorld() {
        guard let worldTilemap else { return }
        
        worldTilemap.clear()
        world.forCoords { coords in
            worldTilemap.setCell(coords: coords, sourceId: 0, atlasCoords: world.at(point: coords).atlasCoords(for: coords))
        }
    }
    
    private func renderUnits() {
        guard let unitsTilemap else { return }
        
        unitsTilemap.clear()
        for unit in units {
            unitsTilemap.setCell(coords: unit.position, sourceId: 0, atlasCoords: unit.kind.tile.atlasCoords(for: unit.position))
        }
    }
    
    private func updateResultLabel() {
        guard let resultLabel else { return }
        
        let goblins = units.filter { $0.kind == .goblin }
        let elves = units.filter { $0.kind == .elf }
        
        switch scheduler.state {
        case .loading:
            resultLabel.text = "\(goblins.count) goblins, \(elves.count) elves"
        case .running, .paused:
            resultLabel.text = "Power: \(state.currentAttackPower), Round \(state.round): \(goblins.count) goblins, \(elves.count) elves"
        case .done:
            let hpLeft = goblins.map(\.health).reduce(0, +) + elves.map(\.health).reduce(0, +)
            resultLabel.text = "Result: \(state.round * hpLeft) (\(state.round) * \(hpLeft)), power: \(state.currentAttackPower)"
        }
    }
    
    private func render() {
        renderUnits()
        updateResultLabel()
    }

    override func _ready() {
        readInput()

        renderWorld()
        render()
        if let worldTilemap, let camera {
            camera.center(on: worldTilemap)
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

private extension Day15 {
    func readInput() {
        let input = FileAccess.getFileAsString(path: "res://input/day15.txt")
        let lines = input.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n").map(Array.init)
        let width = lines.map(\.count).max()!
        
        world = [[Tile]](repeating: [Tile](repeating: .empty, count: width), count: lines.count)
        for (y, line) in lines.enumerated() {
            for (x, c) in line.enumerated() {
                if c == "G" {
                    units.append(Unit(kind: .goblin, position: Vector2i(x: x, y: y)))
                } else if c == "E" {
                    units.append(Unit(kind: .elf, position: Vector2i(x: x, y: y), attackPower: state.currentAttackPower))
                } else {
                    world[y][x] = c == "#" ? .wall : .empty
                }
            }
        }
    }
    
    func firstEnemyInRange(unit: Unit, allUnits: [Vector2i: Unit]) -> Unit? {
        Vector2i.directions()
            .compactMap { direction in
                allUnits[unit.position + direction]
            }
            .filter { other in
                other.kind != unit.kind
            }
            .sorted { lhs, rhs in
                if lhs.health != rhs.health {
                    return lhs.health < rhs.health
                }
                return unitReadingOrderComparator(lhs: lhs, rhs: rhs)
            }
            .first
    }
    
    func attack(source: Unit, target: Unit) -> Bool {
//        GD.print("\(source.position) deals \(source.attackPower) dmg to \(target.position)")
        target.health -= source.attackPower
        return target.health <= 0
    }
    
    func emptyAdjacentTiles(to pos: Vector2i, allUnits: [Vector2i: Unit]) -> [Vector2i] {
        Vector2i.directions()
            .map { $0 + pos }
            .filter { !allUnits.keys.contains($0) }
            .filter { world.safeAt(point: $0) != .wall }
    }
    
    func bfs(start: Vector2i, target: Vector2i, allUnits: [Vector2i: Unit]) -> Int? {
        var visited: Set<Vector2i> = []
        var queue: [Vector2i] = [start]
        var len = 1
        
        if start == target {
            return 0
        }
        
        while !queue.isEmpty {
            var nextQueue: [Vector2i] = []
            for current in queue {
                if visited.contains(current) {
                    continue
                }
                visited.insert(current)
                
                for candidate in emptyAdjacentTiles(to: current, allUnits: allUnits) {
                    if candidate == target {
                        return len
                    }
                    nextQueue.append(candidate)
                }
            }
            queue = nextQueue
            len += 1
        }
        return nil
    }
    
    func moveOpportunities(unit: Unit, target: Vector2i, allUnits: [Vector2i: Unit]) -> (first: Vector2i, len: Int)? {
        emptyAdjacentTiles(to: unit.position, allUnits: allUnits)
            .compactMap { start in
                bfs(start: start, target: target, allUnits: allUnits).map { (first: start, len: $0) }
            }
            .sorted { lhs, rhs in
                if lhs.len != rhs.len {
                    return lhs.len < rhs.len
                }
                return lhs.first.isBeforeInReadingOrder(other: rhs.first)
            }
            .first
    }
    
    func move(unit: Unit, allUnits: [Vector2i: Unit]) -> Bool {
        let enemies = allUnits.values.filter { $0.kind != unit.kind }
        if enemies.isEmpty {
            return false
        }
        
        let validTargets = Set(enemies.flatMap { emptyAdjacentTiles(to: $0.position, allUnits: allUnits) })
        let possiblePaths = validTargets.compactMap { moveOpportunities(unit: unit, target: $0, allUnits: allUnits) }
        
//        GD.print("\(unit.position) wants to move towards \(validTargets)")
//        GD.print("\(unit.position) bfs results: \(possiblePaths)")
        let bestPath = possiblePaths.sorted { lhs, rhs in
            if lhs.len != rhs.len {
                return lhs.len < rhs.len
            }
            return lhs.first.isBeforeInReadingOrder(other: rhs.first)
        }.first
        if let bestPath {
            unit.position = bestPath.first
        }
        return true
    }
    
    func restartGame() {
        units = []
        state.round = 0
        readInput()
        renderWorld()
        render()
    }

    func tick() {
        var sorted = units.sorted(by: unitReadingOrderComparator(lhs:rhs:))
        var allUnits = units.reduce(into: [Vector2i: Unit]()) { partialResult, unit in
            partialResult[unit.position] = unit
        }
        
        var gameEnded = false
        for unit in sorted {
            if unit.health <= 0 {
                continue
            }
            
            var enemy = firstEnemyInRange(unit: unit, allUnits: allUnits)
            if enemy == nil {
                let previousPos = unit.position
                if !move(unit: unit, allUnits: allUnits) {
                    gameEnded = true
                    break
                }
                if unit.position != previousPos {
//                    GD.print("\(previousPos) moves to \(unit.position)")
                    allUnits.removeValue(forKey: previousPos)
                    allUnits[unit.position] = unit
                }
                enemy = firstEnemyInRange(unit: unit, allUnits: allUnits)
            }
            
            if let enemy {
                if attack(source: unit, target: enemy) {
//                    GD.print("\(enemy.position) dies")
                    allUnits.removeValue(forKey: enemy.position)
                    units.remove(at: units.firstIndex(where: { $0.position == enemy.position })!)
                    world.set(at: enemy.position, value: .deadBody)
                    worldTilemap?.setCell(coords: enemy.position, sourceId: 0, atlasCoords: Tile.deadBody.atlasCoords(for: enemy.position))

                    if enemy.kind == .elf {
                        gameEnded = true
                        break
                    }
                }
            }
        }
        
        if gameEnded {
            let victory = !units.contains(where: { $0.kind == .goblin })
            if victory {
                state.elfAttackPowerHigh = state.currentAttackPower
            } else {
                state.elfAttackPowerLow = state.currentAttackPower
            }
            if state.currentAttackPower == (state.elfAttackPowerLow + 1) {
                scheduler.end()
//                GD.print("Best attack power: \(state.currentAttackPower)")
                return
            }
            state.currentAttackPower = state.elfAttackPowerLow + (state.elfAttackPowerHigh - state.elfAttackPowerLow) / 2
            restartGame()
        } else {
//            GD.print("round \(state.round) finishes")
            state.round += 1
        }
    }
}

private extension Vector2i {
    func isBeforeInReadingOrder(other: Vector2i) -> Bool {
        if y != other.y {
            return y < other.y
        }
        return x < other.x
    }
}

private func unitReadingOrderComparator(lhs: Unit, rhs: Unit) -> Bool {
    lhs.position.isBeforeInReadingOrder(other: rhs.position)
}
