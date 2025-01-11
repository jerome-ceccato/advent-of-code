@preconcurrency import SwiftGodot

// Tiles adapted from https://lyime.itch.io/dwarven-delve-asset-pack
private enum Tile: Equatable {
    case empty
    case vertical
    case horizontal
    case upRight
    case upLeft
    case downRight
    case downLeft
    case intersection
    
    init(raw: Character, above: Character?) {
        switch (raw, above) {
        case ("|", _), ("^", _), ("v", _):
            self = .vertical
        case ("-", _), (">", _), ("<", _):
            self = .horizontal
        case ("/", let c) where ["|", "+", "v", "^"].contains(c):
            self = .downLeft
        case ("/", _):
            self = .upRight
        case ("\\", let c) where ["|", "+", "v", "^"].contains(c):
            self = .downRight
        case ("\\", _):
            self = .upLeft
        case ("+", _):
            self = .intersection
        default:
            self = .empty
        }
    }
    
    var atlasCoords: Vector2i {
        switch self {
        case .vertical:
            return Vector2i(x: 0, y: 2)
        case .horizontal:
            return Vector2i(x: 1, y: 2)
        case .upRight:
            return Vector2i(x: 2, y: 2)
        case .upLeft:
            return Vector2i(x: 3, y: 2)
        case .downRight:
            return Vector2i(x: 2, y: 3)
        case .downLeft:
            return Vector2i(x: 3, y: 3)
        case .intersection:
            return Vector2i(x: 1, y: 3)
        default:
            return .zero
        }
    }
}

private final class Cart {
    var position: Vector2i
    var direction: Vector2i
    var turnCount = 0
    
    init(position: Vector2i, direction: Vector2i) {
        self.position = position
        self.direction = direction
    }
}

@Godot
class Day13: Node2D, @unchecked Sendable {
    @SceneTree(path: "TileMapLayer") var tilemap: TileMapLayer?
    @SceneTree(path: "Carts") var cartsContainer: Node2D?
    @SceneTree(path: "Camera2D") var camera: AocCamera?
    @SceneTree(path: "UI/Label") var resultLabel: Label?
    
    @Export var tickSpeed: Int = 30
    @Export var cartScene: PackedScene?
    
    private lazy var scheduler = VisualizationScheduler(
        schedule: .everyNPhysicsTicks(n: tickSpeed),
        tick: { [weak self] in self?.tick() },
        render: { [weak self] in self?.render(animated: true) }
    )
    
    private var world: [[Tile]] = []
    private var carts: [Cart] = []
    private var cartNodes: [Day13Cart] = []
    private var tickCount = 0
    private var crashLocation: Vector2i = Vector2i(x: -1, y: -1)
    
    private func updateTileMap() {
        guard let tilemap else { return }

        tilemap.clear()
        world.forCoords { coords in
            let tile = world.at(point: coords)
            if tile != .empty {
                tilemap.setCell(coords: coords, sourceId: 0, atlasCoords: tile.atlasCoords)
            }
        }
    }
    
    private func updateCarts(animated: Bool) {
        guard let tilemap else { return }
        
        for (cart, node) in zip(carts, cartNodes) {
            if animated {
                getTree()?
                    .createTween()?
                    .tweenProperty(
                        object: node,
                        property: "position",
                        finalVal: Variant(tilemap.mapToLocal(mapPosition: cart.position)),
                        duration: scheduler.ticksLengthInSeconds - 0.01
                    )?
                    .finished.connect {
                        self.updateCartDirection(cart: cart, node: node)
                    }
            } else {
                node.position = tilemap.mapToLocal(mapPosition: cart.position)
                updateCartDirection(cart: cart, node: node)
            }
        }
    }
    
    private func updateCartDirection(cart: Cart, node: Day13Cart) {
        if cart.position == crashLocation {
            node.setCrashed()
        } else {
            node.updateSprite(direction: cart.direction)
        }
    }
    
    private func updateResultLabel() {
        guard let resultLabel else { return }
        
        switch scheduler.state {
        case .loading:
            resultLabel.text = "\(carts.count) carts"
        case .running, .paused:
            resultLabel.text = "Ticks: \(tickCount)"
        case .done:
            resultLabel.text = "Collision: \(crashLocation.x),\(crashLocation.y)"
        }
    }
    
    private func renderAll() {
        updateTileMap()
        render(animated: false)
        
        if let tilemap, let camera {
            camera.center(on: tilemap)
        }
    }
    
    private func render(animated: Bool) {
        updateResultLabel()
        updateCarts(animated: animated)
    }
    
    override func _ready() {
        readInput()
        buildCartNodes()
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

private extension Day13 {
    func readInput() {
        let input = FileAccess.getFileAsString(path: "res://input/day13.txt")
        let lines = input.trimmingCharacters(in: .newlines).components(separatedBy: "\n").map(Array.init)
        let width = lines.map(\.count).max()!
        
        world = [[Tile]](repeating: [Tile](repeating: .empty, count: width), count: lines.count)
        for (y, line) in lines.enumerated() {
            for (x, c) in line.enumerated() {
                if let cartDirection = Vector2i(direction: c) {
                    carts.append(Cart(position: Vector2i(x: x, y: y), direction: cartDirection))
                }
                world[y][x] = Tile(raw: c, above: lines.safeAt(point: Vector2i(x: x, y: y - 1)))
            }
        }
    }
  
    func buildCartNodes() {
        guard let cartsContainer, let cartScene else { return }
        for cart in carts {
            let cartNode = cartScene.instantiate() as! Day13Cart
            cartsContainer.addChild(node: cartNode)
            cartNodes.append(cartNode)
        }
    }
    
    func move(cart: Cart) {
        cart.position += cart.direction
        switch (world.at(point: cart.position), cart.direction) {
        case (.upRight, Vector2i.up):
            cart.direction = Vector2i.right
        case (.upRight, Vector2i.left):
            cart.direction = Vector2i.down
        case (.upLeft, Vector2i.up):
            cart.direction = Vector2i.left
        case (.upLeft, Vector2i.right):
            cart.direction = Vector2i.down
        case (.downRight, Vector2i.down):
            cart.direction = Vector2i.right
        case (.downRight, Vector2i.left):
            cart.direction = Vector2i.up
        case (.downLeft, Vector2i.down):
            cart.direction = Vector2i.left
        case (.downLeft, Vector2i.right):
            cart.direction = Vector2i.up
        case (.intersection, _):
            cart.handleIntersection()
        default:
            break
        }
    }
    
    func hasCollision(cart: Cart) -> Bool {
        for other in carts {
            if other !== cart && other.position == cart.position {
                return true
            }
        }
        return false
    }
  
    func tick() {
        let turnOrder = carts.sorted { lhs, rhs in
            lhs.position < rhs.position
        }
        for cart in turnOrder {
            move(cart: cart)
            if hasCollision(cart: cart) {
                crashLocation = cart.position
                scheduler.end()
                return
            }
        }
        tickCount += 1
    }
}

private extension Cart {
    func handleIntersection() {
        let possiblities: [Vector2i] = [
            direction.turn(offset: -1),
            direction,
            direction.turn(offset: 1)
        ]
        
        direction = possiblities[turnCount % possiblities.count]
        turnCount += 1
    }
}

private extension Vector2i {
    init?(direction: Character) {
        switch direction {
        case "^":
            self = .up
        case ">":
            self = .right
        case "v":
            self = .down
        case "<":
            self = .left
        default:
            return nil
        }
    }
}
