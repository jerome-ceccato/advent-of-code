import Foundation

func readInput() -> [Int: Set<Int>] {
    let path = Bundle.module.path(forResource: "input", ofType: nil)!
    let contents = try! String(contentsOfFile: path, encoding: .utf8)
    let lines = contents.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: .newlines)
    
    return lines.reduce(into: [:]) { acc, line in
        let parts = line.components(separatedBy: " <-> ")
        acc[Int(parts[0])!] = Set(parts[1].components(separatedBy: ", ").compactMap(Int.init))
    }
}

func findAllConnections(from origin: Int, pipes: [Int: Set<Int>]) -> Set<Int> {
    var queue = [origin]
    var result = Set<Int>()

    while let current = queue.popLast() {
        if !result.contains(current) {
            result.insert(current)
            queue.append(contentsOf: pipes[current] ?? [])
        }
    }
    return result
}

func findAllGroups(pipes: [Int: Set<Int>]) -> [Set<Int>] {
    var alreadyUsed = Set<Int>()
    var groups = [Set<Int>]()
    
    for v in pipes.keys {
        if !alreadyUsed.contains(v) {
            let newGroup = findAllConnections(from: v, pipes: pipes)
            groups.append(newGroup)
            alreadyUsed.formUnion(newGroup)
        }
    }
    return groups
}

let pipes = readInput()
print(findAllConnections(from: 0, pipes: pipes).count)
print(findAllGroups(pipes: pipes).count)
