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
                return [(to: .secondLeft, steps: 2), (to: .firstHallway, steps: 2), (to: .secondAmber, steps: 1)]
            case .secondAmber:
                return [(to: .firstAmber, steps: 1)]
            case .firstBronze:
                return [(to: .firstHallway, steps: 2), (to: .secondHallway, steps: 2), (to: .secondBronze, steps: 1)]
            case .secondBronze:
                return [(to: .firstBronze, steps: 1)]
            case .firstCopper:
                return [(to: .secondHallway, steps: 2), (to: .thirdHallway, steps: 2), (to: .secondCopper, steps: 1)]
            case .secondCopper:
                return [(to: .firstCopper, steps: 1)]
            case .firstDesert:
                return [(to: .thirdHallway, steps: 2), (to: .firstRight, steps: 2), (to: .secondDesert, steps: 1)]
            case .secondDesert:
                return [(to: .firstDesert, steps: 1)]
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
            return rawValue <= Position.secondDesert.rawValue
        }
        
        var currentDestination: Int? {
            return isInMainRoom ? rawValue / 2 : nil
        }
        
        var isInFirstRoomOfMain: Bool {
            return isInMainRoom && (rawValue % 2 == 0)
        }
        
        var isInSecondRoomOfMain: Bool {
            return isInMainRoom && (rawValue % 2 == 1)
        }
        
        func offset(_ o: Int) -> Position? {
            return Position(rawValue: rawValue + o)
        }
    }
    
    struct Burrow: Hashable, CustomStringConvertible {
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
                "",
                "#############",
                "#\(char(at: .secondLeft))\(char(at: .firstLeft)).\(char(at: .firstHallway)).\(char(at: .secondHallway)).\(char(at: .thirdHallway)).\(char(at: .firstRight))\(char(at: .secondRight))#",
                "###\(char(at: .firstAmber))#\(char(at: .firstBronze))#\(char(at: .firstCopper))#\(char(at: .firstDesert))###",
                "  #\(char(at: .secondAmber))#\(char(at: .secondBronze))#\(char(at: .secondCopper))#\(char(at: .secondDesert))#",
                "  #########",
                "",
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
    
    func allowedNextPositions(from burrow: Burrow, starting pos: Position, sideRoomsAllowed: Bool) -> [Route] {
        let amphipod = burrow.cells[pos]!
        
        return reachablePositions(from: burrow, starting: pos).filter { route in
            if route.to.isInMainRoom {
                // Can only go to their dedicated main room, can't block another pod who needs to move
                return route.to.currentDestination == amphipod.destination
                    && (route.to.isInSecondRoomOfMain || burrow.cells[route.to.offset(1)!] == amphipod)
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
            if amphipod.destination == pos.currentDestination
                && (pos.isInSecondRoomOfMain || burrow.cells[pos.offset(1)!] == amphipod) {
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
                    print("-> \(current.currentCost)")
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
    
    func parseInput(_ raw: String) -> Burrow {
        let lines = raw.components(separatedBy: "\n")
        let rooms = [3, 5, 7, 9].map { x in
            (first: Amphipod(rawValue: lines[2][x])!, second: Amphipod(rawValue: lines[3][x])!)
        }
        return Burrow(main: rooms)
    }
    
    func part1(rawInput: String) -> CustomStringConvertible {
        let burrow = parseInput(rawInput)
        
        /*
        burrow = nextBurrow(from: burrow, moving: .firstCopper, to: .firstHallway)
        burrow = nextBurrow(from: burrow, moving: .firstBronze, to: .firstCopper)
        burrow = nextBurrow(from: burrow, moving: .secondBronze, to: .secondHallway)
        burrow = nextBurrow(from: burrow, moving: .firstHallway, to: .secondBronze)
        burrow = nextBurrow(from: burrow, moving: .firstAmber, to: .firstBronze)
        print(reachablePositions(from: burrow, starting: .firstAmber))
*/
        print(burrow)
        
        return resolve(burrow: burrow)
    }
    
    func part2(rawInput: String) -> CustomStringConvertible {
        return 0
    }
}
