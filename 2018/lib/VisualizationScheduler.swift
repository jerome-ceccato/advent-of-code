@preconcurrency import SwiftGodot

enum TickSchedule {
    case everyNPhysicsTicks(n: Int)
    case multipleTimesPerPhysicsTicks(n: Int)
}

final class VisualizationScheduler {
    private static let physicsProcessTicksPerSecond = 60
    private let tickSchedule: TickSchedule
    private let tick: () -> Void
    private let render: () -> Void
    
    private var pendingTicks = 0
    public private(set) var running = false // Currently ticking
    public private(set) var hasStarted = false // Has been started at least once, remains true if paused afterwards
    public var done = false // Should stop ticking no matter if the user tries to unpause
    
    init(schedule: TickSchedule, tick: @escaping () -> Void, render: @escaping () -> Void) {
        self.tickSchedule = schedule
        self.tick = tick
        self.render = render
    }
    
    func onPhysicsProcess(delta: Double) {
        guard running && !done else { return }
        
        switch tickSchedule {
        case .everyNPhysicsTicks(let n):
            pendingTicks += 1
            if pendingTicks >= n {
                pendingTicks = 0
                tick()
                render()
            }
        case .multipleTimesPerPhysicsTicks(let n):
            for _ in 0 ..< n {
                tick()
            }
            render()
        }
    }
    
    func onInput(event: InputEvent?, node: Node2D) {
        guard let event else { return }
        
        if event.isActionPressed(action: "start") {
            running = !running
            if running {
                hasStarted = true
            }
        } else if event.isActionPressed(action: "exit") {
            node.getTree()?.changeSceneToFile(path: "res://main.tscn")
        }
    }
}
