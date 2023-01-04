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
    
    struct Room {
        var first: Amphipod?
        var second: Amphipod?
        
        static func empty() -> Room {
            Room(first: nil, second: nil)
        }
    }
    
    struct Burrow: CustomStringConvertible {
        let left: Room
        let right: Room
        
        let main: [Room]
        let hallway: [Amphipod?]
        
        init(main: [Room]) {
            self.left = .empty()
            self.right = .empty()
            self.main = main
            self.hallway = [nil, nil, nil]
        }
        
        var description: String {
            let pod: (Amphipod?) -> Character = { a in
                a.map { $0.rawValue } ?? "."
            }
            let lines = [
                "#############",
                "#\(pod(left.second))\(pod(left.first)).\(pod(hallway[0])).\(pod(hallway[1])).\(pod(hallway[2])).\(pod(right.first))\(pod(right.second))#",
                "###\(pod(main[0].first))#\(pod(main[1].first))#\(pod(main[2].first))#\(pod(main[3].first))###",
                "  #\(pod(main[0].second))#\(pod(main[1].second))#\(pod(main[2].second))#\(pod(main[3].second))#",
                "  #########"
            ]

            return lines.joined(separator: "\n")
        }
        
        var isSolved: Bool {
            return !(0 ..< 4).contains(where: { x in
                if let first = main[x].first, let second = main[x].second {
                    return first.destination != x || second.destination != x
                }
                return false
            })
        }
    }
    
    class Memo {
        var best: Int = Int.max
    }
    
    func availableAmphipods(from burrow: Burrow) -> [Amphipod] {
        return []
    }
    
    func allMoves(from burrow: Burrow, amphipod: Amphipod) -> [(Burrow, Int)] {
        return []
    }
    
    func resolve(burrow: Burrow, currentCost: Int, memo: Memo) -> Int {
        if currentCost > memo.best {
            return memo.best
        }
        
        if burrow.isSolved {
            memo.best = currentCost
            return currentCost
        }
        
        let pods = availableAmphipods(from: burrow)
        for pod in pods {
            let possibilities = allMoves(from: burrow, amphipod: pod)
        }
        
        return Int.max
    }
    
    func parseInput(_ raw: String) -> Burrow {
        let lines = raw.components(separatedBy: "\n")
        let rooms = [3, 5, 7, 9].map { x in
            Room(first: Amphipod(rawValue: lines[2][x]), second: Amphipod(rawValue: lines[3][x]))
        }
        return Burrow(main: rooms)
    }
    
    func part1(rawInput: String) -> CustomStringConvertible {
        let burrow = parseInput(rawInput)
        
        print(burrow)
        return 0
    }
    
    func part2(rawInput: String) -> CustomStringConvertible {
        return 0
    }
}
