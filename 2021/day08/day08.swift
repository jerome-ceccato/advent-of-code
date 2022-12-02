//
//  day08.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 18/12/2021.
//

import Foundation

final class Day08: AOCDay {
    struct Signal {
        let patterns: [String]
        let output: [String]
    }
    
    func parseInput(_ raw: String) -> [Signal] {
        return raw.components(separatedBy: "\n").compactMap { line in
            let parts = line.components(separatedBy: "|")
            return parts.count == 2 ?
                Signal(
                    patterns: parts[0].components(separatedBy: " ").filter { !$0.isEmpty },
                    output: parts[1].components(separatedBy: " ").filter { !$0.isEmpty }
                ) : nil
        }
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        return input.reduce(0) { acc, signal in
            return acc + signal.output.reduce(0, { acc, output in
                return acc + ([2, 3, 4, 7].contains(output.count) ? 1 : 0)
            })
        }
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        func getOutputValue(signal: Signal) -> Int {
            let one = Set(signal.patterns.first { $0.count == 2 }!)
            let seven = Set(signal.patterns.first { $0.count == 3 }!)
            let four = Set(signal.patterns.first { $0.count == 4 }!)
            let eight = Set(signal.patterns.first { $0.count == 7 }!)
            
            func decode(output: Set<Character>) -> Int {
                let known = [one: 1, four: 4, seven: 7, eight: 8]
                
                if known.keys.contains(output) {
                    return known[output]!
                } else if output.count == 5 {
                    if output.intersection(one) == one {
                        return 3
                    } else if output.intersection(four).count == 3 {
                        return 5
                    }
                    return 2
                } else if output.count == 6 {
                    if output.intersection(one) == one && output.intersection(four) == four && output.intersection(seven) == seven {
                        return 9
                    } else if output.intersection(one) == one {
                        return 0
                    }
                    return 6
                }
                fatalError()
            }
            
            return signal.output.reduce(0) { acc, output in
                return acc * 10 + decode(output: Set(output.sorted()))
            }
        }
        
        return input.reduce(0) { acc, signal in
            return acc + getOutputValue(signal: signal)
        }
    }
}
