//
//  day15.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 18/12/2021.
//

import Foundation

final class Day15: AOCDay {
    struct Vector: Equatable, Hashable {
        let x: Int
        let y: Int
    }
    
    func parseInput(_ raw: String) -> [[Int]] {
        return raw.components(separatedBy: "\n")
            .filter { !$0.isEmpty }
            .map { $0.compactMap { Int(String($0)) } }
    }
    
    func shortestPath(input: [[Int]]) -> Int {
        var lastMemo: [Vector: Int] = [:]
        var shortestMemo: [Vector: Int] = [Vector(x: 0, y: 0): 0]
        
        // Dijkstra is probably the expected solution but I wanted to come up with a solution myself
        // My naive solution didn't account for backtracking (going up or left) being faster, so
        // I keep iterating to correct paths until I can't anymore
        
        while lastMemo != shortestMemo {
            lastMemo = shortestMemo
            for y in 0 ..< input.count {
                for x in (y == 0 ? 1 : 0) ..< input[y].count {
                    let left = Vector(x: x - 1, y: y)
                    let up = Vector(x: x, y: y - 1)
                    let right = Vector(x: x + 1, y: y)
                    let down = Vector(x: x, y: y + 1)
                    
                    let minPath = [
                        shortestMemo[left, default: Int.max],
                        shortestMemo[up, default: Int.max],
                        shortestMemo[right, default: Int.max],
                        shortestMemo[down, default: Int.max],
                    ].min()!
                    
                    let thisValue = minPath + input[y][x]
                    shortestMemo[Vector(x: x, y: y)] = thisValue
                }
            }
        }
        
        return shortestMemo[Vector(x: input.last!.count - 1, y: input.count - 1)]!
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        return shortestPath(input: input)
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)

        func enlargeMap(input: [[Int]]) -> [[Int]] {
            var result = [[Int]]()
            
            for yScale in 0 ..< 5 {
                for y in 0 ..< input.count {
                    result.append([])
                    for xScale in 0 ..< 5 {
                        for x in 0 ..< input[y].count {
                            let lastLine = y + (yScale * input.count)
                            let newValue = input[y][x] + yScale + xScale
                            let wrapped = ((newValue - 1) % 9) + 1
                            result[lastLine].append(wrapped)
                        }
                    }
                }
            }
            return result
        }
        
        let largeInput = enlargeMap(input: input)
        return shortestPath(input: largeInput)
    }
}
