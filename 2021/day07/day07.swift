//
//  day07.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 07/12/2021.
//

import Foundation

final class Day07: AOCDay {
    func parseInput(_ raw: String) -> [Int] {
        return raw.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: ",").compactMap(Int.init)
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        func distance(to target: Int, input: [Int]) -> Int {
            return input.reduce(0) { acc, item in
                return acc + abs(item - target)
            }
        }
        
        let positionRange = input.min()! ... input.max()!
        return positionRange.reduce(Int.max) { acc, pos in
            return min(acc, distance(to: pos, input: input))
        }
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        // I was too lazy to figure out that 1+2+3+...+n = (n*(n+1))/2
        var memo = [Int: Int]()
        func fuelCost(for distance: Int) -> Int {
            if distance == 0 {
                return 0
            }
            if let cost = memo[distance] {
                return cost
            }
            memo[distance] = distance + fuelCost(for: distance - 1)
            return memo[distance]!
        }
        
        func distance(to target: Int, input: [Int]) -> Int {
            return input.reduce(0) { acc, item in
                let dist = abs(item - target)
                return acc + fuelCost(for: dist)
            }
        }
        
        let positionRange = input.min()! ... input.max()!
        return positionRange.reduce(Int.max) { acc, pos in
            return min(acc, distance(to: pos, input: input))
        }
    }
}
