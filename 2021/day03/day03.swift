//
//  main.swift
//  day03
//
//  Created by Jerome Ceccato on 03/12/2021.
//

import Foundation

final class Day03: AOCDay {
    func parseInput(_ raw: String) -> [String] {
        return raw.components(separatedBy: "\n").filter { line in !line.isEmpty }
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        var bits = [Int].init(repeating: 0, count: input[0].count)
        input.forEach { number in
            number.enumerated().forEach { item in
                bits[item.offset] += item.element == "0" ? -1 : 1
            }
        }
        
        var gamma = 0, epsilon = 0
        bits.forEach { bitOffset in
            gamma = (gamma << 1) + (bitOffset >= 0 ? 1 : 0)
            epsilon = (epsilon << 1) + (bitOffset >= 0 ? 0 : 1)
        }

        return gamma * epsilon
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        func bitCount(from input: [String], at index: Int) -> Int {
            return input.reduce(0) { acc, line in
                return acc + (line[index] == "0" ? -1 : 1)
            }
        }
        
        func getTargetLine(input: [String], getTarget: (Int) -> Character) -> String {
            var workInput = input
            var bitIndex = 0
            while (workInput.count > 1) {
                let bits = bitCount(from: workInput, at: bitIndex)
                let target = getTarget(bits)
                workInput = workInput.filter { line in line[bitIndex] == target }
                bitIndex += 1
            }
            return workInput.first!
        }
        
        let input = parseInput(rawInput)
        let oxygen = getTargetLine(input: input) { bits in bits >= 0 ? "1" : "0" }
        let co2 = getTargetLine(input: input) { bits in bits < 0 ? "1" : "0" }
        
        return Int(oxygen, radix: 2)! * Int(co2, radix: 2)!
    }
}
