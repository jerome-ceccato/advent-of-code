//
//  day23.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 04/01/2023.
//

import Foundation


final class Day23: AOCDay {
    enum Amphipod: Character, Equatable {
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
    
    enum Position: Int, Equatable {
        case firstAmber
        case secondAmber
        case thirdAmber
        case fourthAmber
        
        case firstBronze
        case secondBronze
        case thirdBronze
        case fourthBronze
        
        case firstCopper
        case secondCopper
        case thirdCopper
        case fourthCopper
        
        case firstDesert
        case secondDesert
        case thirdDesert
        case fourthDesert
        
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
                return [(to: .secondLeft, steps: 2), (to: .firstHallway, steps: 2), (to: .secondAmber, steps: 1)]
            case .secondAmber:
                return [(to: .firstAmber, steps: 1), (to: .thirdAmber, steps: 1)]
            case .thirdAmber:
                return [(to: .secondAmber, steps: 1), (to: .fourthAmber, steps: 1)]
            case .fourthAmber:
                return [(to: .thirdAmber, steps: 1)]
            case .firstBronze:
                return [(to: .firstHallway, steps: 2), (to: .secondHallway, steps: 2), (to: .secondBronze, steps: 1)]
            case .secondBronze:
                return [(to: .firstBronze, steps: 1), (to: .thirdBronze, steps: 1)]
            case .thirdBronze:
                return [(to: .secondBronze, steps: 1), (to: .fourthBronze, steps: 1)]
            case .fourthBronze:
                return [(to: .thirdBronze, steps: 1)]
            case .firstCopper:
                return [(to: .secondHallway, steps: 2), (to: .thirdHallway, steps: 2), (to: .secondCopper, steps: 1)]
            case .secondCopper:
                return [(to: .firstCopper, steps: 1), (to: .thirdCopper, steps: 1)]
            case .thirdCopper:
                return [(to: .secondCopper, steps: 1), (to: .fourthCopper, steps: 1)]
            case .fourthCopper:
                return [(to: .thirdCopper, steps: 1)]
            case .firstDesert:
                return [(to: .thirdHallway, steps: 2), (to: .firstRight, steps: 2), (to: .secondDesert, steps: 1)]
            case .secondDesert:
                return [(to: .firstDesert, steps: 1), (to: .thirdDesert, steps: 1)]
            case .thirdDesert:
                return [(to: .secondDesert, steps: 1), (to: .fourthDesert, steps: 1)]
            case .fourthDesert:
                return [(to: .thirdDesert, steps: 1)]
            case .secondLeft:
                return [(to: .firstLeft, steps: 1)]
            case .firstLeft:
                return [(to: .firstAmber, steps: 2), (to: .firstHallway, steps: 2), (to: .secondLeft, steps: 1)]
            case .firstHallway:
                return [(to: .firstAmber, steps: 2), (to: .firstBronze, steps: 2), (to: .firstLeft, steps: 2), (to: .secondHallway, steps: 2)]
            case .secondHallway:
                return [(to: .firstBronze, steps: 2), (to: .firstCopper, steps: 2), (to: .firstHallway, steps: 2), (to: .thirdHallway, steps: 2)]
            case .thirdHallway:
                return [(to: .firstCopper, steps: 2), (to: .firstDesert, steps: 2), (to: .secondHallway, steps: 2), (to: .firstRight, steps: 2)]
            case .firstRight:
                return [(to: .firstDesert, steps: 2), (to: .thirdHallway, steps: 2), (to: .secondRight, steps: 1)]
            case .secondRight:
                return [(to: .firstRight, steps: 1)]
            }
        }
        
        var isInMainRoom: Bool {
            return rawValue <= Position.fourthDesert.rawValue
        }
        
        var currentDestination: Int? {
            return isInMainRoom ? rawValue / 4 : nil
        }
        
        var nextInRoom: [Position] {
            return ((rawValue % 4 + 1) ..< 4).compactMap { Position(rawValue: ((rawValue / 4) * 4) + $0) }
        }
        
        func offset(_ o: Int) -> Position? {
            return Position(rawValue: rawValue + o)
        }
    }
    
    struct Burrow: Hashable, CustomStringConvertible {
        let cells: [Position: Amphipod]
        
        static let extraItemsFolded: [Position: Amphipod] = [
            .secondAmber: .desert,
            .thirdAmber: .desert,
            .secondBronze: .copper,
            .thirdBronze: .bronze,
            .secondCopper: .bronze,
            .thirdCopper: .amber,
            .secondDesert: .amber,
            .thirdDesert: .copper,
        ]
        
        static let extraItemsFill: [Position: Amphipod] = [
            .thirdAmber: .amber,
            .fourthAmber: .amber,
            .thirdBronze: .bronze,
            .fourthBronze: .bronze,
            .thirdCopper: .copper,
            .fourthCopper: .copper,
            .thirdDesert: .desert,
            .fourthDesert: .desert,
        ]
        
        init(main: [(first: Amphipod, second: Amphipod)], folded: Bool) {
            var items = [Position: Amphipod]()
            main.enumerated().forEach { index, elements in
                items[Position(rawValue: index * 4)!] = elements.first
                items[Position(rawValue: index * 4 + (folded ? 3 : 1))!] = elements.second
            }
            
            if folded {
                for (p, a) in Burrow.extraItemsFolded {
                    items[p] = a
                }
            } else {
                for (p, a) in Burrow.extraItemsFill {
                    items[p] = a
                }
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
                "",
                "#############",
                "#\(char(at: .secondLeft))\(char(at: .firstLeft)).\(char(at: .firstHallway)).\(char(at: .secondHallway)).\(char(at: .thirdHallway)).\(char(at: .firstRight))\(char(at: .secondRight))#",
                "###\(char(at: .firstAmber))#\(char(at: .firstBronze))#\(char(at: .firstCopper))#\(char(at: .firstDesert))###",
                "  #\(char(at: .secondAmber))#\(char(at: .secondBronze))#\(char(at: .secondCopper))#\(char(at: .secondDesert))#",
                "  #\(char(at: .thirdAmber))#\(char(at: .thirdBronze))#\(char(at: .thirdCopper))#\(char(at: .thirdDesert))#",
                "  #\(char(at: .fourthAmber))#\(char(at: .fourthBronze))#\(char(at: .fourthCopper))#\(char(at: .fourthDesert))#",
                "  #########",
                "",
            ]

            return lines.joined(separator: "\n")
        }
        
        var isSolved: Bool {
            let expected: [Amphipod: [Position]] = [
                .amber: [.firstAmber, .secondAmber, .thirdAmber, .fourthAmber],
                .bronze: [.firstBronze, .secondBronze, .thirdBronze, .fourthBronze],
                .copper: [.firstCopper, .secondCopper, .thirdCopper, .fourthCopper],
                .desert: [.firstDesert, .secondDesert, .thirdDesert, .fourthDesert],
            ]
            
            for (amphipod, positions) in expected {
                for pos in positions {
                    if cells[pos] != amphipod {
                        return false
                    }
                }
            }
            return true
        }
    }
    
    typealias Route = (to: Position, steps: Int)
    typealias Move = (burrow: Burrow, cost: Int)
    typealias State = (burrow: Burrow, currentCost: Int)
    
    class Memo {
        var best = Int.max
        var lowestCostToState = [Burrow: Int]()
    }
    
    func nextBurrow(from burrow: Burrow, moving from: Position, to: Position) -> Burrow {
        var newCells = burrow.cells
        newCells[to] = newCells[from]
        newCells.removeValue(forKey: from)
        return Burrow(cells: newCells)
    }
    
    func reachablePositions(from burrow: Burrow, starting pos: Position) -> [Route] {
        var available = [Route]()
        
        var queue = pos.linked.map { [$0] }
        while !queue.isEmpty {
            let item = queue.first!
            let last = item.last!
            
            if burrow.cells[last.to] == nil, !available.contains(where: { $0.to == last.to }) {
                available.append((to: last.to, steps: item.reduce(0, { $0 + $1.steps })))
                queue.append(contentsOf: last.to.linked.map { item + [$0] })
            }
            
            queue.remove(at: 0)
        }
        return available
    }
    
    func mainRoomPosition(_ pos: Position, isCorrectForAmphipod amphipod: Amphipod, in burrow: Burrow) -> Bool {
        if pos.isInMainRoom, amphipod.destination == pos.currentDestination {
            for next in pos.nextInRoom {
                if burrow.cells[next] != amphipod {
                    return false
                }
            }
            return true
        }
        return false
    }
    
    func allowedNextPositions(from burrow: Burrow, starting pos: Position, sideRoomsAllowed: Bool) -> [Route] {
        let amphipod = burrow.cells[pos]!
        
        return reachablePositions(from: burrow, starting: pos).filter { route in
            if route.to.isInMainRoom {
                // Can only go to their dedicated main room, can't block another pod who needs to move
                return mainRoomPosition(route.to, isCorrectForAmphipod: amphipod, in: burrow)
            } else {
                return sideRoomsAllowed
            }
        }
    }
    
    func allMoves(from burrow: Burrow, pos: Position) -> [Move] {
        let amphipod = burrow.cells[pos]!
        
        var routes = [Route]()
        if pos.isInMainRoom {
            // Already in correct position
            if mainRoomPosition(pos, isCorrectForAmphipod: amphipod, in: burrow) {
                return []
            }
            routes = allowedNextPositions(from: burrow, starting: pos, sideRoomsAllowed: true)
        }
        // Is in a side room, can only go to destination
        else {
            routes = allowedNextPositions(from: burrow, starting: pos, sideRoomsAllowed: false)
        }
        
        return routes
            .sorted(by: { a, b in
                a.to.rawValue < b.to.rawValue // Try to go to the main room first, if possible
            })
            .map { route in
                return (burrow: nextBurrow(from: burrow, moving: pos, to: route.to), cost: route.steps * amphipod.cost)
            }
    }
    
    func resolve(burrow original: Burrow) -> Int {
        let memo = Memo()
        var stack: [State] = [(burrow: original, currentCost: 0)]
        
        while !stack.isEmpty {
            let current = stack.popLast()!
            
            if let cached = memo.lowestCostToState[current.burrow], cached <= current.currentCost {
                // Ignore if we've already found a faster way to get to this state
                continue
            } else {
                memo.lowestCostToState[current.burrow] = current.currentCost
            }
            
            if current.currentCost < memo.best {
                if current.burrow.isSolved {
                    memo.best = current.currentCost
                } else {
                    let possibilities = current.burrow.cells.keys.flatMap { pos in
                        allMoves(from: current.burrow, pos: pos)
                    }
                    stack.append(contentsOf: possibilities.map({ route in
                        (burrow: route.burrow, currentCost: current.currentCost + route.cost)
                    }))
                }
            }
        }
        return memo.best
    }
    
    func parseInput(_ raw: String, extraFold: Bool) -> Burrow {
        let lines = raw.components(separatedBy: "\n")
        let rooms = [3, 5, 7, 9].map { x in
            (first: Amphipod(rawValue: lines[2][x])!, second: Amphipod(rawValue: lines[3][x])!)
        }
        return Burrow(main: rooms, folded: extraFold)
    }
    
    func part1(rawInput: String) -> CustomStringConvertible {
        let burrow = parseInput(rawInput, extraFold: false)
        return resolve(burrow: burrow)
    }
    
    func part2(rawInput: String) -> CustomStringConvertible {
        let burrow = parseInput(rawInput, extraFold: true)
        return resolve(burrow: burrow)
    }
}
