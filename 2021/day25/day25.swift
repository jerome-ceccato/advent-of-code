//
//  day25.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 06/01/2023.
//

import Foundation

final class Day25: AOCDay {
    enum Tile: Character {
        case empty = "."
        case east = ">"
        case south = "v"
    }
    
    typealias Board = [[Tile]]
    
    func parseInput(_ raw: String) -> Board {
        return raw.components(separatedBy: "\n")
            .filter { !$0.isEmpty }
            .map { line in
                line.compactMap(Tile.init)
            }
    }
    
    func moveOnce(board: Board) -> Board {
        var next = board
        let height = board.count
        let width = board[0].count
        
        let foreach: ((Int, Int) -> Void) -> Void = { closure in
            for y in 0 ..< height {
                for x in 0 ..< width {
                    closure(x, y)
                }
            }
        }
        
        foreach { x, y in
            if board[y][x] == .east {
                if board[y][(x + 1) % width] == .empty {
                    next[y][(x + 1) % width] = .east
                    next[y][x] = .empty
                }
            }
        }
        
        let temp = next
        foreach { x, y in
            if temp[y][x] == .south {
                if temp[(y + 1) % height][x] == .empty {
                    next[(y + 1) % height][x] = .south
                    next[y][x] = .empty
                }
            }
        }
        
        return next
    }
    
    func stepsUntilSeaCucumbersStop(with original: Board) -> Int {
        var steps = 1
        var currentBoard = original
        
        while true {
            let next = moveOnce(board: currentBoard)
            if next == currentBoard {
                return steps
            }
            currentBoard = next
            steps += 1
        }
    }
    
    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)

        return stepsUntilSeaCucumbersStop(with: input)
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        return "No part 2"
    }
}
