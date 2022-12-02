//
//  day10.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 18/12/2021.
//

import Foundation

final class Day10: AOCDay {
    func parseInput(_ raw: String) -> [String] {
        return raw.components(separatedBy: "\n").map { $0.trimmingCharacters(in: .whitespaces) }.filter { !$0.isEmpty }
    }
    
    let chunks: [Character: Character] = [
        "(": ")",
        "[": "]",
        "{": "}",
        "<": ">",
    ]
    
    func invalidCharacter(for line: String) -> Character? {
        var stack = [Character]()
        
        for char in line {
            if chunks.keys.contains(char) {
                stack.append(char)
            } else {
                if let last = stack.last, chunks[last] == char {
                    stack.removeLast()
                } else {
                    return char
                }
            }
        }
        return nil
    }
    
    func incompleteStack(for line: String) -> [Character] {
        var stack = [Character]()
        
        for char in line {
            if chunks.keys.contains(char) {
                stack.append(char)
            } else {
                // Invalid lines have already been filtered out
                stack.removeLast()
            }
        }
        return stack
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        func points(for line: String) -> Int {
            let pts: [Character: Int] = [
                ")": 3,
                "]": 57,
                "}": 1197,
                ">": 25137,
            ]
            
            return invalidCharacter(for: line).flatMap { c in pts[c] } ?? 0
        }
        
        return input.reduce(0) { acc, line in
            return acc + points(for: line)
        }
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        let incompleteLines = input.filter { invalidCharacter(for: $0) == nil }
        
        func points(for stack: [Character]) -> Int {
            let pts: [Character: Int] = [
                "(": 1,
                "[": 2,
                "{": 3,
                "<": 4,
            ]
            
            return stack.reversed().reduce(0) { acc, char in
                return acc * 5 + pts[char]!
            }
        }
        
        let scores = incompleteLines.map { points(for: incompleteStack(for: $0)) }
        return scores.sorted()[scores.count / 2]
    }
}
