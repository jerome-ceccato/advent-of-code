//
//  day23.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 04/01/2023.
//

import Foundation


final class Day23: AOCDay {
    enum Amphipod: Character {
        case amber = "A"
        case bronze = "B"
        case copper = "C"
        case desert = "D"
        
        var destination: Int {
            return "ABCD".index(of: self.rawValue)!
        }
        
        var cost: Int {
            return [1, 10, 100, 1000][destination]
        }
    }
    
    typealias Route = (to: Position, steps: Int)
    
    enum Position: Int, Equatable {
        case firstAmber
        case secondAmber
        case firstBronze
        case secondBronze
        case firstCopper
        case secondCopper
        case firstDesert
        case secondDesert
        
        case secondLeft
        case firstLeft
        
        case firstHallway
        case secondHallway
        case thirdHallway
        
        case firstRight
        case secondRight
        
        var linked: [Route] {
            switch self {
            case .firstAmber:
                return [(to: .secondLeft, steps: 2), (to: .firstHallway, steps: 2)]
            case .secondAmber:
                return [(to: .firstAmber, steps: 1)]
            case .firstBronze:
                return [(to: .firstHallway, steps: 2), (to: .secondHallway, steps: 2)]
            case .secondBronze:
                return [(to: .firstBronze, steps: 1)]
            case .firstCopper:
                return [(to: .secondHallway, steps: 2), (to: .thirdHallway, steps: 2)]
            case .secondCopper:
                return [(to: .firstCopper, steps: 1)]
            case .firstDesert:
                return [(to: .thirdHallway, steps: 2), (to: .firstRight, steps: 2)]
            case .secondDesert:
                return [(to: .firstDesert, steps: 1)]
            case .secondLeft:
                return [(to: .firstLeft, steps: 1)]
            case .firstLeft:
                return [(to: .firstAmber, steps: 2), (to: .firstHallway, steps: 2)]
            case .firstHallway:
                return [(to: .firstAmber, steps: 2), (to: .firstBronze, steps: 2), (to: .firstLeft, steps: 2), (to: .secondHallway, steps: 2)]
            case .secondHallway:
                return [(to: .firstBronze, steps: 2), (to: .firstCopper, steps: 2), (to: .firstHallway, steps: 2), (to: .thirdHallway, steps: 2)]
            case .thirdHallway:
                return [(to: .firstCopper, steps: 2), (to: .firstDesert, steps: 2), (to: .secondHallway, steps: 2), (to: .firstRight, steps: 2)]
            case .firstRight:
                return [(to: .firstDesert, steps: 2), (to: .thirdHallway, steps: 2)]
            case .secondRight:
                return [(to: .firstRight, steps: 1)]
            }
        }
    }
    
    struct Burrow: CustomStringConvertible {
        let cells: [Position: Amphipod]
        
        init(main: [(first: Amphipod, second: Amphipod)]) {
            var items = [Position: Amphipod]()
            main.enumerated().forEach { index, elements in
                items[Position(rawValue: index * 2)!] = elements.first
                items[Position(rawValue: index * 2 + 1)!] = elements.second
            }
            self.cells = items
        }
        
        init(cells: [Position: Amphipod]) {
            self.cells = cells
        }
        
        private func char(at pos: Position) -> Character {
            cells[pos].map { $0.rawValue } ?? "."
        }
        
        var description: String {
            let lines = [
                "#############",
                "#\(char(at: .secondLeft))\(char(at: .firstLeft)).\(char(at: .firstHallway)).\(char(at: .secondHallway)).\(char(at: .thirdHallway)).\(char(at: .firstRight))\(char(at: .secondRight))#",
                "###\(char(at: .firstAmber))#\(char(at: .firstBronze))#\(char(at: .firstCopper))#\(char(at: .firstDesert))###",
                "  #\(char(at: .secondAmber))#\(char(at: .secondBronze))#\(char(at: .secondCopper))#\(char(at: .secondDesert))#",
                "  #########"
            ]

            return lines.joined(separator: "\n")
        }
        
        var isSolved: Bool {
            return cells[.firstAmber] == .amber && cells[.secondAmber] == .amber
            && cells[.firstBronze] == .bronze && cells[.secondBronze] == .bronze
            && cells[.firstCopper] == .copper && cells[.secondCopper] == .copper
            && cells[.firstDesert] == .desert && cells[.secondDesert] == .desert
        }
    }
    
    class Memo {
        var best: Int = Int.max
    }
    
    typealias Move = (burrow: Burrow, cost: Int)
    
    func nextBurrow(from burrow: Burrow, moving from: Position, to: Position) -> Burrow {
        var newCells = burrow.cells
        newCells[to] = newCells[from]
        newCells.removeValue(forKey: from)
        return Burrow(cells: newCells)
    }
    
    func allMoves(from burrow: Burrow, pos: Position) -> [Move] {
        var available = [Route]()
        
        // This is all wrong, we need to respect all the conditions
        
        var queue = pos.linked.map { [$0] }
        while !queue.isEmpty {
            let item = queue.first!
            
            if burrow.cells[item.last!.to] == nil, !available.contains(where: { $0.to == item.last!.to }) {
                available.append((to: item.last!.to, steps: item.reduce(0, { $0 + $1.steps })))
                queue.append(contentsOf: item.last!.to.linked.map { item + [$0] })
            }
            
            queue.remove(at: 0)
        }
        return available.map { item in
            (burrow: nextBurrow(from: burrow, moving: pos, to: item.to), cost: item.steps)
        }
    }
    
    func resolve(burrow: Burrow, currentCost: Int, memo: Memo) -> Int {
        if currentCost > memo.best {
            return memo.best
        }
        
        if burrow.isSolved {
            memo.best = currentCost
            print("!!!!!")
            return currentCost
        }
        
        print(currentCost)
        //print(burrow)
        
        return burrow.cells.keys.map { pos in
            let possibilities = allMoves(from: burrow, pos: pos)
            return possibilities.map { move in
                resolve(burrow: move.burrow, currentCost: currentCost + move.cost, memo: memo)
            }.min() ?? Int.max
        }.min() ?? Int.max
    }
    
    func parseInput(_ raw: String) -> Burrow {
        let lines = raw.components(separatedBy: "\n")
        let rooms = [3, 5, 7, 9].map { x in
            (first: Amphipod(rawValue: lines[2][x])!, second: Amphipod(rawValue: lines[3][x])!)
        }
        return Burrow(main: rooms)
    }
    
    func part1(rawInput: String) -> CustomStringConvertible {
        let burrow = parseInput(rawInput)
        return resolve(burrow: burrow, currentCost: 0, memo: Memo())
    }
    
    func part2(rawInput: String) -> CustomStringConvertible {
        return 0
    }
}
