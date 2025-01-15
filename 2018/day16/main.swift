import Foundation

enum Instruction: Equatable, CaseIterable {
    case addr
    case addi
    case mulr
    case muli
    case banr
    case bani
    case borr
    case bori
    case setr
    case seti
    case gtir
    case gtri
    case gtrr
    case eqir
    case eqri
    case eqrr
}

struct Example {
    let before: [Int]
    let instructions: [Int]
    let after: [Int]
    
    init(input: String) {
        let re = /Before: *\[([^\]]+)\]\n([^\n]+)\nAfter: *\[([^\]]+)\]/
        let match = try! re.wholeMatch(in: input)!
        
        self.before = match.1.components(separatedBy: ", ").compactMap(Int.init)
        self.instructions = match.2.components(separatedBy: " ").compactMap(Int.init)
        self.after = match.3.components(separatedBy: ", ").compactMap(Int.init)
    }
}

func readInput() -> ([Example], [[Int]]) {
    let path = Bundle.module.path(forResource: "input", ofType: nil)!
    let contents = try! String(contentsOfFile: path, encoding: .utf8)
    let sections = contents.components(separatedBy: "\n\n\n\n")
    
    let examples = sections[0].components(separatedBy: "\n\n").map(Example.init(input:))
    let program = sections[1].trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n").map { line in
        line.components(separatedBy: " ").compactMap(Int.init)
    }
    
    return (examples, program)
}

func process(instruction: Instruction, arguments: [Int], registers: inout [Int]) {
    switch instruction {
    case .addr:
        registers[arguments[2]] = registers[arguments[0]] + registers[arguments[1]]
    case .addi:
        registers[arguments[2]] = registers[arguments[0]] + arguments[1]
    case .mulr:
        registers[arguments[2]] = registers[arguments[0]] * registers[arguments[1]]
    case .muli:
        registers[arguments[2]] = registers[arguments[0]] * arguments[1]
    case .banr:
        registers[arguments[2]] = registers[arguments[0]] & registers[arguments[1]]
    case .bani:
        registers[arguments[2]] = registers[arguments[0]] & arguments[1]
    case .borr:
        registers[arguments[2]] = registers[arguments[0]] | registers[arguments[1]]
    case .bori:
        registers[arguments[2]] = registers[arguments[0]] | arguments[1]
    case .setr:
        registers[arguments[2]] = registers[arguments[0]]
    case .seti:
        registers[arguments[2]] = arguments[0]
    case .gtir:
        registers[arguments[2]] = arguments[0] > registers[arguments[1]] ? 1 : 0
    case .gtri:
        registers[arguments[2]] = registers[arguments[0]] > arguments[1] ? 1 : 0
    case .gtrr:
        registers[arguments[2]] = registers[arguments[0]] > registers[arguments[1]] ? 1 : 0
    case .eqir:
        registers[arguments[2]] = arguments[0] == registers[arguments[1]] ? 1 : 0
    case .eqri:
        registers[arguments[2]] = registers[arguments[0]] == arguments[1] ? 1 : 0
    case .eqrr:
        registers[arguments[2]] = registers[arguments[0]] == registers[arguments[1]] ? 1 : 0
    }
}

func possibleInstructions(for example: Example) -> Set<Instruction> {
    Set(Instruction.allCases.filter({ instruction in
        var registers = example.before
        process(instruction: instruction, arguments: Array(example.instructions[1...]), registers: &registers)
        return registers == example.after
    }))
}

func findOpcodes(examples: [Example]) -> [Int: Instruction] {
    let examplePossibilities = examples.map { ($0.instructions[0], possibleInstructions(for: $0)) }

    // Group by opcode, remove all possibilities that don't match all examples for a given opcode
    var possibilitiesPerOpcode = [Int: Set<Instruction>]()
    for possibility in examplePossibilities {
        if let currentSet = possibilitiesPerOpcode[possibility.0] {
            possibilitiesPerOpcode[possibility.0] = currentSet.intersection(possibility.1)
        } else {
            possibilitiesPerOpcode[possibility.0] = possibility.1
        }
    }
    
    var resolved = [Int: Instruction]()
    while resolved.count < Instruction.allCases.count {
        // Resolve opcodes with a single possible instruction
        for opcode in 0 ..< Instruction.allCases.count {
            if !resolved.keys.contains(opcode) {
                if let instructions = possibilitiesPerOpcode[opcode], instructions.count == 1 {
                    resolved[opcode] = instructions.first!
                    possibilitiesPerOpcode.removeValue(forKey: opcode)
                }
            }
        }
        
        // Remove resolved opcodes from possibility lists
        for opcode in possibilitiesPerOpcode.keys {
            possibilitiesPerOpcode[opcode] = possibilitiesPerOpcode[opcode]!.filter { instruction in
                !resolved.values.contains(instruction)
            }
        }
    }
    
    return resolved
}

func part1(examples: [Example]) -> Int {
    examples
        .map(possibleInstructions(for:))
        .count { $0.count >= 3 }
}

func part2(examples: [Example], program: [[Int]]) -> Int {
    let opcodeMapping = findOpcodes(examples: examples)
    var registers = [0, 0, 0, 0]
    for instruction in program {
        process(instruction: opcodeMapping[instruction[0]]!, arguments: Array(instruction[1...]), registers: &registers)
    }
    return registers[0]
}

let (examples, program) = readInput()
print(part1(examples: examples))
print(part2(examples: examples, program: program))
