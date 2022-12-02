//
//  day09.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 18/12/2021.
//

import Foundation

final class Day09: AOCDay {
    func parseInput(_ raw: String) -> [[Int]] {
        return raw.components(separatedBy: "\n")
            .map { $0.trimmingCharacters(in: .whitespaces) }
            .filter { !$0.isEmpty }
            .map { $0.map(String.init).compactMap(Int.init) }
    }
    
    let surroundingOffsets = [(x: 1, y: 0), (x: -1, y: 0), (x: 0, y: 1), (x: 0, y: -1)]
    
    func isLowPoint(input: [[Int]], x: Int, y: Int) -> Bool {
        let this = input[y][x]
        for offset in surroundingOffsets {
            if let line = input[safe: y + offset.y], let target = line[safe: x + offset.x] {
                if target <= this {
                    return false
                }
            }
        }
        return true
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        func lowPointValue(x: Int, y: Int) -> Int {
            return isLowPoint(input: input, x: x, y: y) ? input[y][x] + 1 : 0
        }
        
        return (0 ..< input.count).reduce(0) { acc, y in
            return acc + (0 ..< input[y].count).reduce(0, { acc2, x in
                return acc2 + lowPointValue(x: x, y: y)
            })
        }
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        var input = parseInput(rawInput)
        
        let lowPoints: [(x: Int, y: Int)] = (0 ..< input.count).reduce(into: []) { acc, y in
            acc.append(contentsOf: (0 ..< input[y].count).reduce(into: [], { acc2, x in
                if isLowPoint(input: input, x: x, y: y) {
                    acc2.append((x: x, y: y))
                }
            }))
        }
        
        func getBasinSize(from: (x: Int, y: Int)) -> Int {
            let this = input[from.y][from.x]
            input[from.y][from.x] = 9 // To avoid counting it multiple times
            
            return surroundingOffsets.reduce(1) { acc, offset in
                let targetPos = (x: from.x + offset.x, y: from.y + offset.y)
                if let targetLine = input[safe: targetPos.y],
                   let target = targetLine[safe: targetPos.x],
                   target > this && target < 9 {
                    return acc + getBasinSize(from: (x: targetPos.x, y: targetPos.y))
                }
                return acc
            }
        }
        
        let basins = lowPoints.map(getBasinSize(from:))
        return basins.sorted(by: >).prefix(3).reduce(1, *)
    }
}
