//
//  day18.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 18/12/2021.
//

import Foundation

final class Day18: AOCDay {
    class Pair {
        var left: Node
        var right: Node
        
        init(left: Node, right: Node) {
            self.left = left
            self.right = right
        }
    }
    
    class Node {
        // This could have been an Enum if it supported mutating associated values
        var number: Int?
        var pair: Pair?
        
        init(number: Int) {
            self.number = number
            self.pair = nil
        }
        
        init(pair: Pair) {
            self.number = nil
            self.pair = pair
        }
    }
    
    func parse(pair: String) -> Pair {
        func parseNode(input: inout String) -> Node {
            if input.hasPrefix("[") {
                return Node(pair: parsePair(input: &input))
            } else {
                return Node(number: Int(String(input.removeFirst()))!)
            }
        }
        
        func parsePair(input: inout String) -> Pair {
            input.removeFirst() // '['
            let left = parseNode(input: &input)
            input.removeFirst() // ','
            let right = parseNode(input: &input)
            input.removeFirst() // ']'
            return Pair(left: left, right: right)
        }
        
        var input = pair
        return parsePair(input: &input)
    }
    
    func parseInput(_ raw: String) -> [Pair] {
        return raw.components(separatedBy: "\n")
            .map { $0.trimmingCharacters(in: .whitespaces) }
            .filter { !$0.isEmpty }
            .map(parse(pair:))
    }
    
    func reduce(pair: Pair) -> Pair {
        func explode(node: Node, parents: [Pair] = []) -> Bool {
            if node.number != nil {
                return false
            } else if let pair = node.pair {
                return explode(pair: pair, parents: parents)
            } else {
                fatalError()
            }
        }

        func add(n: Int, to: Node, left: Bool) {
            // Traverse the tree to add n to the left/rightmost number
            if let number = to.number {
                to.number = number + n
            } else if let pair = to.pair {
                if left {
                    return add(n: n, to: pair.left, left: left)
                } else {
                    return add(n: n, to: pair.right, left: left)
                }
            }
        }
        
        func goAdd(n: Int, pair: Pair, parents: [Pair], left: Bool) {
            // Traverse the tree back, to find the node to traverse for the addition
            var pair = pair
            for parent in parents {
                if left && parent.left.pair !== pair {
                    return add(n: n, to: parent.left, left: !left)
                } else if !left && parent.right.pair !== pair {
                    return add(n: n, to: parent.right, left: !left)
                }
                pair = parent
            }
        }
        
        func explode(pair: Pair, parents: [Pair] = []) -> Bool {
            if parents.count >= 4,
               let left = pair.left.number,
               let right = pair.right.number {
                let parent = parents.last!
                if parent.left.pair === pair {
                    parent.left.number = 0
                    parent.left.pair = nil
                    goAdd(n: left, pair: parent, parents: parents.prefix(parents.count - 1).reversed(), left: true)
                    add(n: right, to: parent.right, left: true)
                } else if parent.right.pair === pair {
                    parent.right.number = 0
                    parent.right.pair = nil
                    add(n: left, to: parent.left, left: false)
                    goAdd(n: right, pair: parent, parents: parents.prefix(parents.count - 1).reversed(), left: false)
                } else {
                    fatalError()
                }
                return true
            }
            let updatedParents = parents + [pair]
            return explode(node: pair.left, parents: updatedParents) || explode(node: pair.right, parents: updatedParents)
        }
        
        func split(node: Node) -> Bool {
            if let n = node.number {
                if n > 9 {
                    node.number = nil
                    node.pair = Pair(left: Node(number: n / 2), right: Node(number: n / 2 + n % 2))
                    return true
                } else {
                    return false
                }
            } else if let pair = node.pair {
                return split(pair: pair)
            } else {
                fatalError()
            }
        }
        func split(pair: Pair) -> Bool {
            return split(node: pair.left) || split(node: pair.right)
        }
        
        while explode(pair: pair) || split(pair: pair) {}
        return pair
    }
    
    func magnitude(node: Node) -> Int {
        if let n = node.number {
            return n
        } else if let pair = node.pair {
            return magnitude(pair: pair)
        } else {
            fatalError()
        }
    }
    
    func magnitude(pair: Pair) -> Int {
        return 3 * magnitude(node: pair.left) + 2 * magnitude(node: pair.right)
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        let result = input.suffix(from: 1).reduce(input[0]) { acc, newPair in
            return reduce(pair: Pair(left: Node(pair: acc), right: Node(pair: newPair)))
        }
        return magnitude(pair: result)
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        var magnitudes = Set<Int>()
        let inputSize = parseInput(rawInput).count
        for i in 0 ..< inputSize {
            for j in 0 ..< inputSize {
                if i != j {
                    // Load the input each time because the reduction is mutating it
                    let input = parseInput(rawInput)
                    let addition = reduce(pair: Pair(left: Node(pair: input[i]), right: Node(pair: input[j])))
                    magnitudes.insert(magnitude(pair: addition))
                }
            }
        }
        
        return magnitudes.max()!
    }
}
