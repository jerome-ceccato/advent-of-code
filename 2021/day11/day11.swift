//
//  day11.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 18/12/2021.
//

import Foundation

final class Day11: AOCDay {
    func parseInput(_ raw: String) -> [Int] {
        return raw.components(separatedBy: "\n").flatMap { $0.trimmingCharacters(in: .whitespaces) }.compactMap { Int(String($0)) }
    }
    
    func flash(input: inout [Int], at position: Int) -> [Int] {
        let posX = position % 10, posY = position / 10
        let offsets: [(x: Int, y: Int)] = [
            (x: -1, y: -1),
            (x: 0, y: -1),
            (x: 1, y: -1),
            (x: -1, y: 0),
            (x: 1, y: 0),
            (x: -1, y: 1),
            (x: 0, y: 1),
            (x: 1, y: 1),
        ]
        
        return offsets.compactMap { offset in
            let validCoordinate = (0 ..< 10)
            let targetX = posX + offset.x
            let targetY = posY + offset.y
            if validCoordinate.contains(targetX) && validCoordinate.contains(targetY) {
                let target = targetX + (10 * targetY)
                input[target] += 1
                if input[target] == 10 {
                    return target
                }
            }
            return nil
        }
    }
    
    func stepOnce(input: inout [Int]) -> Int {
        input = input.map { $0 + 1 }
        
        var flashes = 0
        var queue = (0 ..< input.count).filter { input[$0] > 9 }
        while !queue.isEmpty {
            let pos = queue.removeFirst()
            queue.append(contentsOf: flash(input: &input, at: pos))
            flashes += 1
        }
        
        input = input.map { $0 > 9 ? 0 : $0 }
        return flashes
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        var input = parseInput(rawInput)
        
        return (0 ..< 100).reduce(0) { acc, _ in
            return acc + stepOnce(input: &input)
        }
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        var input = parseInput(rawInput)
        
        return (1...).first { _ in
            return stepOnce(input: &input) == 100
        }!
    }
}
