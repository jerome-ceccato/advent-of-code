@preconcurrency import SwiftGodot



final class VisualizationScheduler {
    enum Schedule {
        case everyNPhysicsTicks(n: Int)
        case multipleTimesPerPhysicsTicks(n: Int)
    }
    
    enum State: Equatable {
        case loading
        case running
        case paused
        case done
    }
    
    private static let physicsProcessTicksPerSecond = 60
    private let tickSchedule: Schedule
    private let tick: () -> Void
    private let render: () -> Void
    
    private var pendingTicks = 0
    public private(set) var state = State.loading
    
    init(schedule: Schedule, tick: @escaping () -> Void, render: @escaping () -> Void) {
        self.tickSchedule = schedule
        self.tick = tick
        self.render = render
    }
    
    func end() {
        state = .done
    }
    
    var ticksLengthInSeconds: Double {
        switch tickSchedule {
        case .everyNPhysicsTicks(let n):
            return Double(n) / Double(Self.physicsProcessTicksPerSecond)
        case .multipleTimesPerPhysicsTicks(let n):
            return 1.0 / Double(Self.physicsProcessTicksPerSecond * n)
        }
    }
    
    func onPhysicsProcess(delta: Double) {
        guard state == .running else { return }
        
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
            switch state {
            case .loading:
                state = .running
            case .running:
                state = .paused
            case .paused:
                state = .running
            case .done:
                break
            }
        } else if event.isActionPressed(action: "exit") {
            node.getTree()?.changeSceneToFile(path: "res://main.tscn")
        }
    }
}
