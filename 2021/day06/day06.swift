//
//  main.swift
//  day05
//
//  Created by Jerome Ceccato on 05/12/2021.
//

import Foundation

final class Day06: AOCDay {
    func parseInput(_ raw: String) -> [Int] {
        return raw.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: ",").compactMap(Int.init)
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        func tickOnce(_ world: [Int]) -> [Int] {
            var result = [Int]()
            world.forEach { fish in
                if fish == 0 {
                    result.append(contentsOf: [6, 8])
                } else {
                    result.append(fish - 1)
                }
            }
            return result
        }
        
        let world = (0 ..< 80).reduce(input) { world, _ in
            return tickOnce(world)
        }
        return world.count
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        let flatWorld = input.reduce(into: [:]) { acc, i in
            acc[i, default: 0] += 1
        }

        func tickOnce(_ world: [Int: Int]) -> [Int: Int] {
            var result = [Int: Int]()
            world.keys.forEach { key in
                if key == 0 {
                    result[6, default: 0] += world[key]!
                    result[8, default: 0] += world[key]!
                } else {
                    result[key - 1, default: 0] += world[key]!
                }
            }
            return result
        }
        
        let world = (0 ..< 256).reduce(flatWorld) { world, _ in
            return tickOnce(world)
        }
        return world.values.reduce(0, +)
    }
}
