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
        var tested: Set<String> = Set()
        
        func identifier(for alu: ALU, instructions: Int) -> String {
            return alu.registries.map { String($0) }.joined(separator: "-") + "-\(instructions)"
        }
    }
    
    func input(alu: ALU, into: Registry, current: [Int], instructions: [Instruction], memo: Memo) -> [Int]? {
        var alu = alu
        if current.count <= 3 {
            print("input \(current)")
        }
        for input in (1 ... 9).reversed() {
            alu.registries[into.rawValue] = input
            if let res = resolve(alu: alu, instructions: instructions, current: current + [input], memo: memo) {
                return res
            }
        }
        return nil
    }
    
    func resolve(alu: ALU, instructions: [Instruction], current: [Int], memo: Memo) -> [Int]? {
        var alu = alu
        //print(current)
        for (i, instruction) in instructions.enumerated() {
            
            // If we've already encountered this exact pattern, no need to continue
            let identifier = memo.identifier(for: alu, instructions: instructions.count - i)
            if memo.tested.contains(identifier) {
                return nil
            }
            memo.tested.insert(identifier)
            
            switch instruction {
            case .input(let into):
                return input(alu: alu, into: into, current: current, instructions: Array(instructions[(i + 1)...]), memo: memo)
            case .add(let left, let right):
                alu.registries[left.rawValue] += right.get(with: alu.registries)
            case .mul(let left, let right):
                alu.registries[left.rawValue] *= right.get(with: alu.registries)
            case .div(let left, let right):
                if right.get(with: alu.registries) == 0 {
                    return nil
                }
                alu.registries[left.rawValue] /= right.get(with: alu.registries)
            case .mod(let left, let right):
                if alu.registries[left.rawValue] < 0 || right.get(with: alu.registries) <= 0 {
                    return nil
                }
                alu.registries[left.rawValue] %= right.get(with: alu.registries)
            case .equal(let left, let right):
                alu.registries[left.rawValue] = alu.registries[left.rawValue] == right.get(with: alu.registries) ? 1 : 0
            }
        }
        
        return alu.registries[Registry.z.rawValue] == 0 ? current : nil
    }
    
    typealias SingleStep = (alus: Set<ALU>, nextInput: Int)
    func solveOne(alu original: ALU, instructions: [Instruction]) -> SingleStep {
        guard let first = instructions.first,
              case let .input(into) = first else { return (alus: Set(), nextInput: -1) }
        
        var result = Set<ALU>()
        var nextInput = -1
        for input in (1 ... 9).reversed() {
            var alu = original
            var valid = true
            alu.registries[into.rawValue] = input
            
        loop: for i in 1 ..< instructions.count {
                switch instructions[i] {
                case .input:
                    nextInput = i
                    break loop
                case .add(let left, let right):
                    alu.registries[left.rawValue] += right.get(with: alu.registries)
                case .mul(let left, let right):
                    alu.registries[left.rawValue] *= right.get(with: alu.registries)
                case .div(let left, let right):
                    if right.get(with: alu.registries) == 0 {
                        valid = false
                        break loop
                    }
                    alu.registries[left.rawValue] /= right.get(with: alu.registries)
                case .mod(let left, let right):
                    if alu.registries[left.rawValue] < 0 || right.get(with: alu.registries) <= 0 {
                        valid = false
                        break loop
                    }
                    alu.registries[left.rawValue] %= right.get(with: alu.registries)
                case .equal(let left, let right):
                    alu.registries[left.rawValue] = alu.registries[left.rawValue] == right.get(with: alu.registries) ? 1 : 0
                }
            }
            
            if valid {
                result.insert(alu)
            }
        }
        
        return (alus: result, nextInput: nextInput)
    }
    
    func solveStep(from alu: ALU, instructions: [Instruction]) {
        let step = solveOne(alu: alu, instructions: instructions)
        //print("from \(alu), \(step.alus.count) steps")
        if step.alus.count < 9 {
            print("from \(alu), \(step.alus.count) steps: \(step.alus)")
        }
        if step.nextInput > 0 && step.nextInput < instructions.count {
            let nextInstructions = Array(instructions[step.nextInput...])
            for next in step.alus {
                solveStep(from: next, instructions: nextInstructions)
            }
        }
    }
    
    func test(seq: [Int], instructions: [Instruction]) -> Bool {
        var alu = ALU()
        var seqi = 0

        for instruction in instructions {
            switch instruction {
            case .input(let into):
                alu.registries[into.rawValue] = seq[seqi]
                seqi += 1
            case .add(let left, let right):
                alu.registries[left.rawValue] += right.get(with: alu.registries)
            case .mul(let left, let right):
                alu.registries[left.rawValue] *= right.get(with: alu.registries)
            case .div(let left, let right):
                if right.get(with: alu.registries) == 0 {
                    return false
                }
                alu.registries[left.rawValue] /= right.get(with: alu.registries)
            case .mod(let left, let right):
                if alu.registries[left.rawValue] < 0 || right.get(with: alu.registries) <= 0 {
                    return false
                }
                alu.registries[left.rawValue] %= right.get(with: alu.registries)
            case .equal(let left, let right):
                alu.registries[left.rawValue] = alu.registries[left.rawValue] == right.get(with: alu.registries) ? 1 : 0
            }
        }
        
        return alu.registries[Registry.z.rawValue] == 0
    }
    
    func findMonad(instructions: [Instruction]) -> [Int] {
        return resolve(alu: ALU(), instructions: instructions, current: [], memo: Memo())!
    }
    
    func parseInput(_ raw: String) -> [Instruction] {
        return raw.components(separatedBy: "\n").compactMap(Instruction.init)
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        //[1, 1, 8, 4, 1, 2, 3, 1, 1, 1, 7, 1, 8, 9]
        // 11841231117189 too low
        // input [9, 2, 3]
        //print(test(seq: [1, 1, 8, 4, 1, 2, 3, 1, 1, 1, 7, 1, 8, 9], instructions: input))
        //return findMonad(instructions: input)
        solveStep(from: ALU(), instructions: input)
        return -1
    }

    func part2(rawInput: String) -> CustomStringConvertible {
//        let input = parseInput(rawInput)
        return -1
    }
}
