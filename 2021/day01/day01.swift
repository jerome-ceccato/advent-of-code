//
//  main.swift
//  day01
//
//  Created by Jerome Ceccato on 01/12/2021.
//

import Foundation

final class Day01: AOCDay {
    func parseInput(_ raw: String) -> [Int] {
        return raw.components(separatedBy: "\n").compactMap(Int.init)
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        var n = 0
        for i in 1 ..< input.count {
            n += (input[i] > input[i - 1]) ? 1 : 0
        }
        return n
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        var n = 0
        for i in 3 ..< input.count {
            n += (input[i] > input[i - 3]) ? 1 : 0
        }
        return n
    }
}
