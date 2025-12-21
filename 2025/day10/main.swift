import Foundation

struct Machine {
    let pattern: UInt
    let buttons: [UInt]
    let counterButtons: [[Int]]
    let joltage: [Int]
}

func parsePattern(raw: String) -> UInt {
    raw.reversed().reduce(0) { acc, c in
        acc << 1 | (c == "#" ? 1 : 0)
    }
}

func parseButton(raw: String) -> [Int] {
    raw.components(separatedBy: ",").compactMap(Int.init)
}

func parseButtons(raw: String) -> [[Int]] {
    let regex = /\(([^\)]+)\)/
    return raw.matches(of: regex).map { match in
        parseButton(raw: String(match.output.1))
    }
}

func parseJoltage(raw: String) -> [Int] {
    raw.components(separatedBy: ",").compactMap(Int.init)
}

func parseMachine(line: String) -> Machine? {
    let regex = /\[([^\]]+)\] ([^\{]+) {([^}]+)}/
    guard let match = try? regex.firstMatch(in: line) else {
        return nil
    }

    let rawButtons = parseButtons(raw: String(match.output.2))
    return Machine(
        pattern: parsePattern(raw: String(match.output.1)),
        buttons: rawButtons.map { $0.reduce(0) { acc, rank in acc | (1 << rank) } },
        counterButtons: rawButtons,
        joltage: parseJoltage(raw: String(match.output.3))
    )
}

func parseInput(path: String = "input") -> [Machine] {
    let path = Bundle.module.path(forResource: "input", ofType: nil)!
    let contents = try! String(contentsOfFile: path, encoding: .utf8)

    return contents.components(separatedBy: "\n").compactMap(parseMachine(line:))
}

func findInitSequence(for machine: Machine) -> Int {
    var visited: Set<UInt> = []
    var queue: [(pattern: UInt, history: Int)] = [(pattern: 0, history: 0)]
    var nextQueue: [(pattern: UInt, history: Int)] = []

    while !queue.isEmpty {
        for current in queue {
            if visited.insert(current.pattern).inserted == false {
                continue
            }

            for button in machine.buttons {
                let next = current.pattern ^ button
                let history = current.history + 1
                if next == machine.pattern {
                    return history
                } else {
                    nextQueue.append((pattern: next, history: history))
                }
            }
        }
        queue = nextQueue
        nextQueue = []
    }
    return 0
}

// Output the linear eq to feed to z3
func outputJoltageSequence(for machine: Machine) -> String {
    var result = "["
    for (pos, jolt) in machine.joltage.enumerated() {
        result += "(["
        for (offset, button) in machine.counterButtons.enumerated() {
            if button.contains(pos) {
                let letter = Character(UnicodeScalar(97 + offset)!)
                result += "\"\(letter)\","
            }
        }
        result += "], \(jolt)),"
    }
    result += "],"
    return result
}

let machines = parseInput()
let initSequences = machines.map(findInitSequence(for:))
print(initSequences.reduce(0, +))

let joltageSequences = machines.map(outputJoltageSequence(for:)).joined(separator: "\n")
print(joltageSequences)
