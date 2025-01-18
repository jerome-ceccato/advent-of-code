
public enum Instruction: String, Equatable, CaseIterable {
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

public extension Instruction {
    func process(arguments: [Int], registers: inout [Int]) {
        switch self {
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
}
