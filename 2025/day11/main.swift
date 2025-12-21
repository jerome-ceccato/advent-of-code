import Foundation

func parseInput(path: String = "input") -> [String: [String]] {
    let path = Bundle.module.path(forResource: "input", ofType: nil)!
    let contents = try! String(contentsOfFile: path, encoding: .utf8)

    return contents.components(separatedBy: "\n").reduce(into: [:]) { acc, line in
        let parts = line.components(separatedBy: ": ")
        acc[parts[0]] = parts[1].components(separatedBy: " ")
    }
}

func countPaths(
    from current: String,
    to goal: String,
    network: [String: [String]]
) -> Int {
    var cache = [String: Int]()
    return countPaths(from: current, to: goal, network: network, cache: &cache)
}

func countPaths(
    from current: String,
    to goal: String,
    network: [String: [String]],
    cache: inout [String: Int]
) -> Int {
    if current == goal {
        return 1
    } else if let cached = cache[current] {
        return cached
    }

    let subPaths = network[current, default: []].reduce(0) { acc, next in
        acc + countPaths(from: next, to: goal, network: network, cache: &cache)
    }
    cache[current] = subPaths
    return subPaths
}

let network = parseInput()
print(countPaths(from: "you", to: "out", network: network))

let svrToFft = countPaths(from: "svr", to: "fft", network: network)
let fftToDac = countPaths(from: "fft", to: "dac", network: network)
let dacToOut = countPaths(from: "dac", to: "out", network: network)
print(svrToFft * fftToDac * dacToOut)
