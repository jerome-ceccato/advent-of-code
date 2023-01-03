//
//  day22.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 18/12/2021.
//

import Foundation

final class Day22: AOCDay {
    struct Instruction: Equatable {
        let state: Bool
        let x: ClosedRange<Int>
        let y: ClosedRange<Int>
        let z: ClosedRange<Int>
    }
    
    func parseInput(_ raw: String) -> [Instruction] {
        return raw.components(separatedBy: "\n")
            .map { $0.trimmingCharacters(in: .whitespaces) }
            .filter { !$0.isEmpty }
            .map { line in
                let parts = line.components(separatedBy: " ")
                let state = parts[0] == "on"
                let ranges = parts[1].components(separatedBy: ",")
                    .map { $0.components(separatedBy: "=")[1] }
                    .map { $0.components(separatedBy: "..").compactMap(Int.init) }
                    .map { $0[0] ... $0[1] }
                
                return Instruction(
                    state: state,
                    x: ranges[0],
                    y: ranges[1],
                    z: ranges[2]
                )
            }
    }
    
    func isInsideInitArea(instruction: Instruction) -> Bool {
        let ranges = [instruction.x, instruction.y, instruction.z]
        return ranges.allSatisfy { range in
            return range.lowerBound >= -50 && range.upperBound <= 50
        }
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        let initialInstructions = input.filter(isInsideInitArea(instruction:))
        var board = [[[Bool]]](repeating: [[Bool]](repeating: [Bool](repeating: false, count: 101), count: 101), count: 101)

        func apply(instruction: Instruction, on board: inout [[[Bool]]]) {
            for x in instruction.x {
                for y in instruction.y {
                    for z in instruction.z {
                        board[x + 50][y + 50][z + 50] = instruction.state
                    }
                }
            }
        }
        
        func countActive(board: [[[Bool]]]) -> Int {
            board.reduce(0) { acc1, square in
                return acc1 + square.reduce(0, { acc2, line in
                    return acc2 + line.reduce(0, { acc3, value in
                        return acc3 + (value ? 1 : 0)
                    })
                })
            }
        }
                
        initialInstructions.forEach { instruction in
            apply(instruction: instruction, on: &board)
        }
        
        return countActive(board: board)
    }
    
    func part2(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        let rebootInstructions = input
        var board = [Instruction]()
        
        // returns the intersection of both cubes, with a reversed state
        func intersection(left: Instruction, right: Instruction) -> Instruction? {
            func intersectionRange(left: ClosedRange<Int>, right: ClosedRange<Int>) -> ClosedRange<Int>? {
                let lower = max(left.lowerBound, right.lowerBound)
                let upper = min(left.upperBound, right.upperBound)
                
                return lower <= upper ? lower ... upper : nil
            }
            
            if let rx = intersectionRange(left: left.x, right: right.x),
               let ry = intersectionRange(left: left.y, right: right.y),
               let rz = intersectionRange(left: left.z, right: right.z) {
                return Instruction(state: !left.state, x: rx, y: ry, z: rz)
            }
            
            return nil
        }
        
        func countActive(board: [Instruction]) -> Int {
            return board.reduce(0) { acc, instruction in
                return acc + [instruction.x, instruction.y, instruction.z].map { $0.count }.reduce(instruction.state ? 1 : -1, *)
            }
        }
        
        rebootInstructions.forEach { instruction in
            var next = [Instruction]()
            for existing in board {
                if let intesect = intersection(left: existing, right: instruction) {
                    next.append(intesect)
                }
            }
            if instruction.state {
                board.append(instruction)
            }
            board.append(contentsOf: next)
        }

        return countActive(board: board)
    }
}
