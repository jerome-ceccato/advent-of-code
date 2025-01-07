@preconcurrency import SwiftGodot

enum TickSchedule {
    case everyNPhysicsTicks(n: Int)
    case multipleTimesPerPhysicsTicks(n: Int)
}

final class VisualizationScheduler {
    private static let physicsProcessTicksPerSecond = 60
    private let tickSchedule: TickSchedule
    private let tick: () -> Void
    
    private var pendingTicks = 0
    private var running = false
    public var done = false
    
    init(schedule: TickSchedule, tick: @escaping () -> Void) {
        self.tickSchedule = schedule
        self.tick = tick
    }
    
    func onPhysicsProcess(delta: Double) {
        guard running && !done else { return }
        
        switch tickSchedule {
        case .everyNPhysicsTicks(let n):
            pendingTicks += 1
            if pendingTicks >= n {
                pendingTicks = 0
                tick()
            }
        case .multipleTimesPerPhysicsTicks(let n):
            for _ in 0 ..< n {
                tick()
            }
        }
    }
    
    func onInput(event: InputEvent?) {
        guard let event else { return }
        
        if event.isActionPressed(action: "start") {
            running = !running
        }
    }
}
