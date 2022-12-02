//
//  day21.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 18/12/2021.
//

import Foundation

final class Day21: AOCDay {
    class Dice {
        var rolls = 0
        var value = 0
        
        func roll() -> Int {
            rolls += 1
            value = (value % 100) + 1
            return value
        }
        
        func roll3() -> Int {
            return roll() + roll() + roll()
        }
    }
    
    class Player {
        var position: Int
        var score: Int = 0
        
        init(pos: Int) {
            position = pos
        }
    }
    
    func parseInput(_ raw: String) -> (p1: Int, p2: Int) {
        let positions = raw.trimmingCharacters(in: .whitespacesAndNewlines)
            .components(separatedBy: "\n")
            .compactMap { line in
                Int(line.components(separatedBy: ":")[1].trimmingCharacters(in: .whitespaces))
            }
        return (p1: positions[0] - 1, p2: positions[1] - 1)
    }
    
    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        let player1 = Player(pos: input.p1)
        let player2 = Player(pos: input.p2)
        let dice = Dice()
        
        var hasWinner: Bool {
            return player1.score >= 1000 || player2.score >= 1000
        }
        
        func gameScore() -> Int {
            if player1.score >= 1000 {
                return player2.score * dice.rolls
            } else {
                return player1.score * dice.rolls
            }
        }
        
        while !hasWinner {
            for player in [player1, player2] {
                let roll = dice.roll3()
                player.position = (player.position + roll) % 10
                player.score += player.position + 1
                
                if hasWinner {
                    break
                }
            }
        }
        return gameScore()
    }
    
    struct PlayerState: Hashable, Equatable {
        let position: Int
        let score: Int
    }
    
    struct GameState: Hashable, Equatable {
        let p1: PlayerState
        let p2: PlayerState
    }
    
    func part2(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        var gameStates = [GameState(
            p1: PlayerState(position: input.p1, score: 0),
            p2: PlayerState(position: input.p2, score: 0)): 1]
        
        func newState(from state: PlayerState, roll: Int) -> PlayerState {
            let position = (state.position + roll) % 10
            let score = state.score + position + 1
            return PlayerState(position: position, score: score)
        }
        
        var universe1 = 0
        var universe2 = 0
        
        func runOnce(game: [GameState: Int], turn: Int) -> [GameState: Int] {
            var newGame = [GameState: Int]()
            
            for firstRoll in 1 ... 3 {
                for secondRoll in 1 ... 3 {
                    for thirdRoll in 1 ... 3 {
                        for state in game {
                            let rollValue = firstRoll + secondRoll + thirdRoll
                            if turn == 0 {
                                let newState = newState(from: state.key.p1, roll: rollValue)
                                newGame[GameState(p1: newState, p2: state.key.p2), default: 0] += state.value
                            } else {
                                let newState = newState(from: state.key.p2, roll: rollValue)
                                newGame[GameState(p1: state.key.p1, p2: newState), default: 0] += state.value
                            }
                        }
                    }
                }
            }
            
            for k in newGame.keys {
                if k.p1.score >= 21 {
                    universe1 += newGame[k, default: 0]
                    newGame.removeValue(forKey: k)
                } else if k.p2.score >= 21 {
                    universe2 += newGame[k, default: 0]
                    newGame.removeValue(forKey: k)
                }
            }
            
            return newGame
        }
        
        while !gameStates.isEmpty {
            gameStates = runOnce(game: gameStates, turn: 0)
            gameStates = runOnce(game: gameStates, turn: 1)
        }

        return max(universe1, universe2)
    }
}
