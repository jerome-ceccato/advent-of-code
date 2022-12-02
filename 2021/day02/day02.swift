//
//  main.swift
//  day02
//
//  Created by Jerome Ceccato on 01/12/2021.
//

import Foundation

final class Day02: AOCDay {
    typealias Command = (direction: String, value: Int)

    func parseInput(_ raw: String) -> [Command] {
        return raw.components(separatedBy: "\n").compactMap { line in
            let command = line.components(separatedBy: " ")
            guard command.count == 2, let value = Int(command[1]) else {
                return nil
            }
            return (direction: command[0], value: value)
        }
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        var x = 0, z = 0
        input.forEach { command in
            switch (command.direction) {
            case "forward":
                x += command.value
            case "down":
                z += command.value
            case "up":
                z -= command.value
            default:
                break
            }
        }

        return x * z
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        var x = 0, z = 0, aim = 0
        input.forEach { command in
            switch (command.direction) {
            case "forward":
                x += command.value
                z += aim * command.value
            case "down":
                aim += command.value
            case "up":
                aim -= command.value
            default:
                break
            }
        }
        return x * z
    }
}
