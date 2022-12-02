//
//  main.swift
//  day04
//
//  Created by Jerome Ceccato on 04/12/2021.
//

import Foundation

final class Day04: AOCDay {
    struct Input {
        class Item {
            let value: Int
            var marked: Bool
            
            init(value: Int) {
                self.value = value
                self.marked = false
            }
        }
        typealias Board = [Item]
        
        let draws: [Int]
        let boards: [Board]
    }
    
    func markBoards(boards: [Input.Board], number: Int) {
        for board in boards {
            for item in board {
                if item.value == number {
                    item.marked = true
                }
            }
        }
    }

    func checkLine(board: Input.Board, positions: [Int]) -> Bool {
        for pos in positions {
            if !board[pos].marked {
                return false
            }
        }
        return true
    }

    func checkWinner(boards: [Input.Board]) -> Int? {
        for (index, board) in boards.enumerated() {
            for i in 0 ..< 5 {
                let linePos = (0 ..< 5).map { i * 5 + $0 }
                let columnPos = (0 ..< 5).map { i + $0 * 5 }
                if (checkLine(board: board, positions: linePos) || checkLine(board: board, positions: columnPos)) {
                    return index
                }
            }
        }
        return nil
    }
    
    func parseInput(_ raw: String) -> Input {
        let chunks = raw.components(separatedBy: "\n\n")
        
        let draws = chunks[0].components(separatedBy: ",").compactMap(Int.init)
        let boards = chunks.suffix(from: 1).map { chunk in
            return chunk
                .components(separatedBy: "\n")
                .flatMap { $0.components(separatedBy: " ") }
                .compactMap(Int.init)
                .map(Input.Item.init)
        }
        
        return Input(draws: draws, boards: boards)
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        for draw in input.draws {
            markBoards(boards: input.boards, number: draw)
            
            if let winnerIndex = checkWinner(boards: input.boards) {
                let winner = input.boards[winnerIndex]
                let unmarked = winner.filter { !$0.marked }.map { $0.value }
                let total = unmarked.reduce(0, +)
                return total * draw
            }
        }
        fatalError()
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        var boards = input.boards
        for draw in input.draws {
            markBoards(boards: boards, number: draw)
            
            while let winnerIndex = checkWinner(boards: boards) {
                if boards.count == 1 {
                    let unmarked = boards[0].filter { !$0.marked }.map { $0.value }
                    let total = unmarked.reduce(0, +)
                    return total * draw
                } else {
                    boards.remove(at: winnerIndex)
                }
            }
        }
        fatalError()
    }
}
