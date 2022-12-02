//
//  day14.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 18/12/2021.
//

import Foundation

final class Day14: AOCDay {
    struct Formula {
        typealias PairInsertion = (first: Character, second: Character, insertion: Character)

        let template: [Character]
        let pairInsertions: [PairInsertion]
    }
    
    func parseInput(_ raw: String) -> Formula {
        let chunks = raw.components(separatedBy: "\n\n")
        let template = chunks[0].trimmingCharacters(in: .whitespacesAndNewlines)
        let pairs: [Formula.PairInsertion] = chunks[1].components(separatedBy: "\n")
            .map { $0.trimmingCharacters(in: .whitespaces) }
            .filter { !$0.isEmpty }
            .map { line in
                let items = line.components(separatedBy: " -> ")
                return (first: items[0][0], second: items[0][1], insertion: items[1][0])
            }
        
        return Formula(template: Array(template), pairInsertions: pairs)
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        func insertOnce(input: [Character], pairs: [Formula.PairInsertion]) -> [Character] {
            var next = [Character]()
            
            for i in 0 ..< (input.count - 1) {
                next.append(input[i])
                if let insertion = pairs.first(where: { insertion in
                    return insertion.first == input[i] && insertion.second == input[i + 1]
                }) {
                    next.append(insertion.insertion)
                }
            }
            next.append(input[input.count - 1])
            return next
        }
        
        let result = (0 ..< 10).reduce(input.template) { template, _ in
            return insertOnce(input: template, pairs: input.pairInsertions)
        }
        let mappedResult = result.reduce(into: [:]) { acc, c in
            acc[c, default: 0] += 1
        }
        
        return mappedResult.values.max()! - mappedResult.values.min()!
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        var pairsCount = [String: Int]()
        for i in 0 ..< (input.template.count - 1) {
            let pair = String([input.template[i], input.template[i + 1]])
            pairsCount[pair, default: 0] += 1
        }
        
        func insertOnce(input: [String: Int], pairs: [Formula.PairInsertion]) -> [String: Int] {
            var next = [String: Int]()

            input.forEach { key, value in
                if let insertion = pairs.first(where: { pair in
                    return pair.first == key[0] && pair.second == key[1]
                }) {
                    next[String([insertion.first, insertion.insertion]), default: 0] += value
                    next[String([insertion.insertion, insertion.second]), default: 0] += value
                } else {
                    next[key, default: 0] += value
                }
            }
            return next
        }
        
        let result = (0 ..< 40).reduce(pairsCount) { pairs, _ in
            return insertOnce(input: pairs, pairs: input.pairInsertions)
        }
        
        let mappedResult = result.keys.reduce(into: [input.template.last!: 1]) { acc, key in
            acc[key[0], default: 0] += result[key]!
        }
        
        return mappedResult.values.max()! - mappedResult.values.min()!
    }
}
