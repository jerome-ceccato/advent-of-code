//
//  main.swift
//  day05
//
//  Created by Jerome Ceccato on 05/12/2021.
//

import Foundation

final class Day05: AOCDay {
    typealias Vector = [Int]

    func parseInput(_ raw: String) -> [Vector] {
        let lines = raw.components(separatedBy: "\n").filter { !$0.isEmpty }
        
        return lines.map { line in
            return line.components(separatedBy: "->").flatMap { rawVector in
                return rawVector.trimmingCharacters(in: .whitespaces).components(separatedBy: ",").compactMap(Int.init)
            }
        }
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        let x0 = 0, y0 = 1, x1 = 2, y1 = 3
        var board = [Int](repeating: 0, count: 1000 * 1000)
        
        input.forEach { vector in
            if vector[x0] == vector[x1] {
                let starty = min(vector[y0], vector[y1])
                let endy = max(vector[y0], vector[y1])
                for y in starty ... endy {
                    board[vector[x0] + (y * 1000)] += 1
                }
            } else if vector[y0] == vector[y1] {
                let startx = min(vector[x0], vector[x1])
                let endx = max(vector[x0], vector[x1])
                for x in startx ... endx {
                    board[x + (vector[y0] * 1000)] += 1
                }
            }
        }
        
        let total = board.reduce(0) { acc, count in
            return acc + (count > 1 ? 1 : 0)
        }
        return total
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        let x0 = 0, y0 = 1, x1 = 2, y1 = 3
        var board = [Int](repeating: 0, count: 1000 * 1000)
        
        input.forEach { vector in
            if vector[x0] == vector[x1] {
                let starty = min(vector[y0], vector[y1])
                let endy = max(vector[y0], vector[y1])
                for y in starty ... endy {
                    board[vector[x0] + (y * 1000)] += 1
                }
            } else if vector[y0] == vector[y1] {
                let startx = min(vector[x0], vector[x1])
                let endx = max(vector[x0], vector[x1])
                for x in startx ... endx {
                    board[x + (vector[y0] * 1000)] += 1
                }
            } else {
                let shiftX = vector[x0] < vector[x1] ? 1 : -1
                let shiftY = vector[y0] < vector[y1] ? 1 : -1
                
                var nextX = vector[x0]
                var nextY = vector[y0]
                while nextX != vector[x1] {
                    board[nextX + (nextY * 1000)] += 1
                    nextX += shiftX
                    nextY += shiftY
                }
                board[nextX + (nextY * 1000)] += 1
            }
        }
        
        let total = board.reduce(0) { acc, count in
            return acc + (count > 1 ? 1 : 0)
        }
        return total
    }
}
