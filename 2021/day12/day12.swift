//
//  day12.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 18/12/2021.
//

import Foundation

final class Day12: AOCDay {
    class Node: Equatable {
        let name: String
        var neighbors: [Node] = []
        
        init(name: String) {
            self.name = name
        }
        
        var small: Bool {
            return name.lowercased() == name
        }
        
        var special: Bool {
            return name == "start" || name == "end"
        }
        
        static func == (lhs: Day12.Node, rhs: Day12.Node) -> Bool {
            return lhs.name == rhs.name
        }
    }
    
    func asNodes(raw: [(from: String, to: String)]) -> Node {
        var nodes = [String: Node]()
        
        raw.forEach { path in
            if nodes[path.from] == nil {
                nodes[path.from] = Node(name: path.from)
            }
            if nodes[path.to] == nil {
                nodes[path.to] = Node(name: path.to)
            }
            nodes[path.from]!.neighbors.append(nodes[path.to]!)
            nodes[path.to]!.neighbors.append(nodes[path.from]!)
        }
        return nodes["start"]!
    }
    
    func parseInput(_ raw: String) -> Node {
        return asNodes(raw: raw.components(separatedBy: "\n").compactMap { line in
            let items = line.components(separatedBy: "-").map { $0.trimmingCharacters(in: .whitespaces) }
            return items.count == 2 ? (from: items[0], to: items[1]) : nil
        })
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        func traverse(node: Node, previous: [Node]) -> Int {
            if node.name == "end" {
                return 1
            }
            return node.neighbors.reduce(0) { acc, other in
                if !other.small || !previous.contains(other) {
                    return acc + traverse(node: other, previous: previous + [other])
                }
                return acc
            }
        }
        
        return traverse(node: input, previous: [input])
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        func traverse(node: Node, previous: [Node], canRevisitSmall: Bool) -> Int {
            if node.name == "end" {
                return 1
            }
            return node.neighbors.reduce(0) { acc, other in
                if !other.small || !previous.contains(other) {
                    return acc + traverse(node: other, previous: previous + [other], canRevisitSmall: canRevisitSmall)
                } else if other.small && canRevisitSmall && !other.special {
                    return acc + traverse(node: other, previous: previous + [other], canRevisitSmall: false)
                }
                return acc
            }
        }
        
        return traverse(node: input, previous: [input], canRevisitSmall: true)
    }
}
