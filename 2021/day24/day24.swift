//
//  day24.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 05/01/2023.
//

import Foundation

final class Day24: AOCDay {
    enum Registry: Int {
        case w
        case x
        case y
        case z
        
        init?(raw: String) {
            let mapping: [String: Registry] = ["w": .w, "x": .x, "y": .y, "z": .z]
            if let parsed = mapping[raw] {
                self = parsed
            } else {
                return nil
            }
        }
    }
    
    enum Value {
        case registry(reg: Registry)
        case number(n: Int)
        
        init?(raw: String) {
            if let reg = Registry(raw: raw) {
                self = .registry(reg: reg)
            } else if let n = Int(raw) {
                self = .number(n: n)
            } else {
                return nil
            }
        }
        
        func get(with registries: [Int]) -> Int {
            switch self {
            case .registry(let reg):
                return registries[reg.rawValue]
            case .number(let n):
                return n
            }
        }
    }
    
    enum Instruction {
        case input(into: Registry)
        case add(left: Registry, right: Value)
        case mul(left: Registry, right: Value)
        case div(left: Registry, right: Value)
        case mod(left: Registry, right: Value)
        case equal(left: Registry, right: Value)
        
        init?(raw: String) {
            let components = raw.components(separatedBy: " ")
            switch components[0] {
            case "inp":
                self = .input(into: Registry(raw: components[1])!)
            case "add":
                self = .add(left: Registry(raw: components[1])!, right: Value(raw: components[2])!)
            case "mul":
                self = .mul(left: Registry(raw: components[1])!, right: Value(raw: components[2])!)
            case "div":
                self = .div(left: Registry(raw: components[1])!, right: Value(raw: components[2])!)
            case "mod":
                self = .mod(left: Registry(raw: components[1])!, right: Value(raw: components[2])!)
            case "eql":
                self = .equal(left: Registry(raw: components[1])!, right: Value(raw: components[2])!)
            default:
                return nil
            }
        }
    }
    
    struct ALU: Hashable, CustomStringConvertible {
        var registries = [0, 0, 0, 0]
        
        var description: String {
            return "w: \(registries[0]), x: \(registries[1]), y: \(registries[2]), z: \(registries[3])"
        }
    }
    
    class Memo {
        var tested = [String: Int]()
        
        func identifier(for alu: ALU, instructions: Int) -> String {
            return alu.registries.map { String($0) }.joined(separator: "-") + "-\(instructions)"
        }
    }
    
    func input(alu: ALU, into: Registry, current: [Int], instructions: [Instruction], memo: Memo, reversed: Bool) -> [Int]? {
        var alu = alu
        
        // Cleanup the least used elements of the memo so it doesn't quickly become unmanageable
        if current.count == 3 {
            for key in memo.tested.keys {
                if memo.tested[key]! < 3 {
                    memo.tested.removeValue(forKey: key)
                }
            }
        }
        
        let range = reversed ? Array((1 ... 9).reversed()) : Array(1 ... 9)
        for input in range {
            alu.registries[into.rawValue] = input
            if let res = resolve(alu: alu, instructions: instructions, current: current + [input], memo: memo, reversed: reversed) {
                return res
            }
        }
        return nil
    }
    
    enum ExecutionState: Equatable {
        case ok
        case invalid
        case input(into: Registry)
    }
    
    func execute(instruction: Instruction, in alu: inout ALU) -> ExecutionState {
        switch instruction {
        case .input(let into):
            return .input(into: into)
        case .add(let left, let right):
            alu.registries[left.rawValue] += right.get(with: alu.registries)
        case .mul(let left, let right):
            alu.registries[left.rawValue] *= right.get(with: alu.registries)
        case .div(let left, let right):
            if right.get(with: alu.registries) == 0 {
                return .invalid
            }
            alu.registries[left.rawValue] /= right.get(with: alu.registries)
        case .mod(let left, let right):
            if alu.registries[left.rawValue] < 0 || right.get(with: alu.registries) <= 0 {
                return .invalid
            }
            alu.registries[left.rawValue] %= right.get(with: alu.registries)
        case .equal(let left, let right):
            alu.registries[left.rawValue] = alu.registries[left.rawValue] == right.get(with: alu.registries) ? 1 : 0
        }
        return .ok
    }
    
    func resolve(alu: ALU, instructions: [Instruction], current: [Int], memo: Memo, reversed: Bool) -> [Int]? {
        var alu = alu

        for (i, instruction) in instructions.enumerated() {
            
            // If we've already encountered this exact pattern, no need to continue
            let identifier = memo.identifier(for: alu, instructions: instructions.count - i)
            if let hits = memo.tested[identifier] {
                memo.tested[identifier] = hits + 1
                return nil
            }
            memo.tested[identifier] = 1
            
            switch execute(instruction: instruction, in: &alu) {
            case .input(let into):
                return input(
                    alu: alu,
                    into: into,
                    current: current,
                    instructions: Array(instructions[(i + 1)...]),
                    memo: memo,
                    reversed: reversed
                )
            case .invalid:
                return nil
            case .ok:
                break
            }
        }
        
        return alu.registries[Registry.z.rawValue] == 0 ? current : nil
    }
    
    func test(seq: [Int], instructions: [Instruction]) -> Bool {
        var alu = ALU()
        var seqi = 0

        for instruction in instructions {
            switch execute(instruction: instruction, in: &alu) {
            case .input(let into):
                alu.registries[into.rawValue] = seq[seqi]
                seqi += 1
            case .invalid:
                return false
            case .ok:
                break
            }
        }
        
        return alu.registries[Registry.z.rawValue] == 0
    }
    
    func findMonad(reversed: Bool, instructions: [Instruction]) -> [Int] {
        return resolve(alu: ALU(), instructions: instructions, current: [], memo: Memo(), reversed: reversed)!
    }
    
    func parseInput(_ raw: String) -> [Instruction] {
        return raw.components(separatedBy: "\n").compactMap(Instruction.init)
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)

        return findMonad(reversed: true, instructions: input)
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)

        return findMonad(reversed: false, instructions: input)
    }
}
