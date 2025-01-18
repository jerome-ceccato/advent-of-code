import Foundation
import Device

struct Command {
    let instruction: Instruction
    let parameters: [Int]
    
    init(input: String) {
        let re = /([a-z]+) (\d+) (\d+) (\d+)/
        let match = try! re.wholeMatch(in: input)!
        
        self.instruction = Instruction(rawValue: String(match.1))!
        self.parameters = [Int(match.2)!, Int(match.3)!, Int(match.4)!]
    }
}

func processCommands(
    ipRegister: Int,
    firstRegisterValue: Int,
    commands: [Command],
    exitEarlyAtIp: Int? = nil
) -> [Int] {
    var ip = 0
    var registers = [Int](repeating: 0, count: 6)
    registers[0] = firstRegisterValue
    
    while commands.indices.contains(ip), ip != exitEarlyAtIp {
        let command = commands[ip]

        registers[ipRegister] = ip
        command.instruction.process(arguments: command.parameters, registers: &registers)
        ip = registers[ipRegister]
        ip += 1
    }

    return registers
}

func runDecodedProgram(given z: Int) -> Int {
    // The program finds 2 values b,d such as b*d == z, adding d to the result
    // Essentially it finds all divisors of the number z and add them together
    var divisors: Set<Int> = []
    for i in 1 ... z {
        if z % i == 0 {
            divisors.insert(i)
        }
    }

    return divisors.reduce(0, +)
}

func readInput() -> (ip: Int, commands: [Command]) {
    let path = Bundle.module.path(forResource: "input", ofType: nil)!
    let contents = try! String(contentsOfFile: path, encoding: .utf8)
        .trimmingCharacters(in: .whitespacesAndNewlines)
    let sections = contents.split(separator: "\n", maxSplits: 1)

    let ipRe = /#ip (\d+)/
    let ip = Int(try! ipRe.wholeMatch(in: sections[0])!.1)!

    let commands = sections[1].components(separatedBy: "\n").map(Command.init(input:))
    return (ip: ip, commands: commands)
}

let (ip, commands) = readInput()

let part1 = processCommands(ipRegister: ip, firstRegisterValue: 0, commands: commands)
print(part1[0])

let part2 = processCommands(ipRegister: ip, firstRegisterValue: 1, commands: commands, exitEarlyAtIp: 1)
print(runDecodedProgram(given: part2[5]))

/*
 a = 0
 z = 10551364
 
 d = b
 do {
     b = 1
     do {
         if d * b == z {
            a += d
         }
         b += 1
     } while b <= z
     d += 1
 } while d <= z
 
 
 pseudo code:
 for d in 0 ... z {
    for b in 0 ... z {
        if d * b == z {
            a += d
        }
    }
 }
 return a
 
 */

// reg[0] = a, reg[1] = b, reg[2] = c
// reg[3] = d, reg[4] = ip, reg[5] = z
/*
 * __: a = 1
 *
 * 00: ip += 16
 * 01: d = b
 * 02: b = 1
 * 03: c = d * b
 * 04: c = c == z
 * 05: ip += c
 * 06: ip += 1
 * 07: a += d
 * 08: b += 1
 * 09: c = b > z
 * 10: ip += c
 * 11: ip = 2
 * 12: d += 1
 * 13: c = d > z
 * 14: ip += c
 * 15: ip = 1
 * 16: ip *= ip
 * 17: z += 2
 * 18: z *= z
 * 19: z = ip * z
 * 20: z *= 11
 * 21: c += 5
 * 22: c *= ip
 * 23: c += 18
 * 24: z += c
 * 25: ip += a
 * 26: ip = 0
 * 27: c = ip
 * 28: c *= ip
 * 29: c += ip
 * 30: c *= ip
 * 31: c *= 14
 * 32: c *= ip
 * 33: z += c
 * 34: a = 0
 * 35: ip = 0
 */
