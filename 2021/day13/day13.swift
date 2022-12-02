//
//  day13.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 18/12/2021.
//

import Foundation

final class Day13: AOCDay {
    struct Vector: Hashable, Equatable {
        let x: Int
        let y: Int
    }
    
    enum Fold {
        case foldX(x: Int)
        case foldY(y: Int)
    }
    
    struct Instructions {
        let board: [Vector]
        let folds: [Fold]
    }
    
    func parseInput(_ raw: String) -> Instructions {
        let chunks = raw.components(separatedBy: "\n\n")
        
        let boardItems = chunks[0].components(separatedBy: "\n")
            .map { $0.trimmingCharacters(in: .whitespaces) }
            .filter { !$0.isEmpty }
        let board: [Vector] = boardItems.map { line in
            let coords = line.components(separatedBy: ",").compactMap(Int.init)
            return Vector(x: coords[0], y: coords[1])
        }
        
        let folds: [Fold] = chunks[1].components(separatedBy: "\n")
            .map { $0.trimmingCharacters(in: .whitespaces) }
            .filter { !$0.isEmpty }
            .compactMap { line in
                let lineComponents = line.components(separatedBy: "=")
                if lineComponents[0] == "fold along x" {
                    return Fold.foldX(x: Int(lineComponents[1])!)
                } else if lineComponents[0] == "fold along y" {
                    return Fold.foldY(y: Int(lineComponents[1])!)
                } else {
                    return nil
                }
            }
        
        return Instructions(board: board, folds: folds)
    }
    
    func foldOnce(board: Set<Vector>, fold: Fold) -> Set<Vector> {
        return board.reduce(into: Set<Vector>()) { acc, vector in
            switch fold {
            case .foldX(let x):
                if vector.x <= x {
                    acc.insert(vector)
                } else {
                    acc.insert(Vector(x: x * 2 - vector.x, y: vector.y))
                }
                break
            case .foldY(let y):
                if vector.y <= y {
                    acc.insert(vector)
                } else {
                    acc.insert(Vector(x: vector.x, y: y * 2 - vector.y))
                }
                break
            }
            
        }
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        return foldOnce(board: Set(input.board), fold: input.folds.first!).count
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        let board = input.folds.reduce(Set(input.board), foldOnce)
        
        func boardRepresentation(board: Set<Vector>) -> String {
            let boardSize = Vector(x: board.max(by: { a, b in a.x < b.x })!.x,
                                   y: board.max(by: { a, b in a.y < b.y })!.y)
            var content = "\n"
            
            for y in 0 ... boardSize.y {
                for x in 0 ... boardSize.x {
                    content += board.contains(Vector(x: x, y: y)) ? "#" : " "
                }
                content += "\n"
            }
            return content
        }
        return boardRepresentation(board: board)
    }
}
