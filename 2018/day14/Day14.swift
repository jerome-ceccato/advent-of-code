@preconcurrency import SwiftGodot

@Godot
class Day14: Node2D, @unchecked Sendable {
    @SceneTree(path: "Label") var resultLabel: Label?
    
    @Export var tickSpeed: Int = 10_000
  
    private lazy var scheduler = VisualizationScheduler(
        schedule: .multipleTimesPerPhysicsTicks(n: tickSpeed),
        tick: { [weak self] in self?.tick() },
        render: { [weak self] in self?.render() }
    )
    
    private var target = ""
    private var targetDigits: [Int] = []
    private var board: [Int] = [3, 7]
    private var firstElf = 0
    private var secondElf = 1
    private var result = -1
    
    private func render() {
        guard let resultLabel else { return }
        
        switch scheduler.state {
        case .loading:
            resultLabel.text = "Target: \(target)"
        case .running, .paused:
            resultLabel.text = "Recipes: \(board.count)"
        case .done:
            resultLabel.text = "\(result)"
        }
    }
  
    override func _ready() {
        target = readInput()
        setup()
        render()
        
        super._ready()
    }
    
    override func _physicsProcess(delta: Double) {
        scheduler.onPhysicsProcess(delta: delta)
    }
    
    override func _unhandledInput(event: InputEvent?) {
        scheduler.onInput(event: event, node: self)
    }
}

private extension Day14 {
    func readInput() -> String {
        let input = FileAccess.getFileAsString(path: "res://input/day14.txt")
        return input.trimmingCharacters(in: .whitespacesAndNewlines)
    }
    
    func setup() {
        board.reserveCapacity(1_000_000)
        targetDigits = target.map { Int("\($0)")! }
    }
    
    func checkTarget() -> Bool {
        for i in 0 ..< targetDigits.count {
            if i >= board.count {
                return false
            }
            if targetDigits[targetDigits.count - 1 - i] != board[board.count - 1 - i] {
                return false
            }
        }
        return true
    }
    
    func endIfTargetIsFound() {
        if scheduler.state != .done, checkTarget() {
            scheduler.end()
            result = board.count - targetDigits.count
            GD.print(result)
        }
    }
  
    func tick() {
        let nextRecipe = board[firstElf] + board[secondElf]
        if (nextRecipe / 10) > 0 {
            board.append(nextRecipe / 10)
            endIfTargetIsFound()
        }
        board.append(nextRecipe % 10)
        endIfTargetIsFound()
        firstElf = (firstElf + board[firstElf] + 1) % board.count
        secondElf = (secondElf + board[secondElf] + 1) % board.count
    }
}
