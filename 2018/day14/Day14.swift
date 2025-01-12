@preconcurrency import SwiftGodot

@Godot
class Day14: Node2D, @unchecked Sendable {
    @SceneTree(path: "Label") var resultLabel: Label?
  
    private lazy var scheduler = VisualizationScheduler(
        schedule: .multipleTimesPerPhysicsTicks(n: 10_000),
        tick: { [weak self] in self?.tick() },
        render: { [weak self] in self?.render() }
    )
    
    private var target = -1
    private var board: [Int] = [3, 7]
    private var firstElf = 0
    private var secondElf = 1
    private var result = ""
    
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
    func readInput() -> Int {
        let input = FileAccess.getFileAsString(path: "res://input/day14.txt")
        return Int(input.trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0
    }
    
    func setup() {
        board.reserveCapacity(target + 12)
    }
    
    func extractResult() -> String {
        var res = ""
        for i in target ..< (target + 10) {
            res.append("\(board[i])")
        }
        return res
    }
  
    func tick() {
        if board.count >= (target + 10) {
            scheduler.end()
            result = extractResult()
            GD.print(result)
        } else {
            let nextRecipe = board[firstElf] + board[secondElf]
            if (nextRecipe / 10) > 0 {
                board.append(nextRecipe / 10)
            }
            board.append(nextRecipe % 10)
            firstElf = (firstElf + board[firstElf] + 1) % board.count
            secondElf = (secondElf + board[secondElf] + 1) % board.count
        }
//        GD.print(firstElf, secondElf, board)
    }
}
