//
//  day16.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 18/12/2021.
//

import Foundation

final class Day16: AOCDay {
    enum PacketOperator: Int {
        case sum = 0
        case product = 1
        case min = 2
        case max = 3
        case gt = 5
        case lt = 6
        case eq = 7
    }
    
    enum PacketType {
        case number(n: Int)
        case op(op: PacketOperator, subpackets: [Packet])
    }
            
    struct Packet {
        let version: Int
        let type: PacketType
    }
    
    func parseInput(_ raw: String) -> String {
        return raw.trimmingCharacters(in: .whitespacesAndNewlines).map { char in
            let hex = Int(String(char), radix: 16)!
            let binary = String(hex, radix: 2)
            return (binary.count < 4 ? String(repeating: "0", count: 4 - binary.count) : "") + binary
        }.joined(separator: "")
    }
    
    func consume(bits: Int, input: inout String) -> String {
        let prefix = input.prefix(bits)
        input.removeSubrange(input.startIndex ..< input.index(input.startIndex, offsetBy: prefix.count))
        return String(prefix)
    }
    
    func consumeAsInt(bits: Int, input: inout String) -> Int {
        return Int(consume(bits: bits, input: &input), radix: 2)!
    }
    
    func consumeLiteralValue(input: inout String) -> Int {
        var valueBits = ""
        
        while consume(bits: 1, input: &input) == "1" {
            valueBits += consume(bits: 4, input: &input)
        }
        valueBits += consume(bits: 4, input: &input)
        return Int(valueBits, radix: 2)!
    }
   
    func decode(input: inout String) -> Packet {
        let version = consumeAsInt(bits: 3, input: &input)
        let type = consumeAsInt(bits: 3, input: &input)

        if type == 4 {
            return Packet(version: version, type: .number(n: consumeLiteralValue(input: &input)))
        } else if let opType = PacketOperator(rawValue: type) {
            var subpackets = [Packet]()
            if consumeAsInt(bits: 1, input: &input) == 0 {
                let length = consumeAsInt(bits: 15, input: &input)
                var subInput = consume(bits: length, input: &input)
                while !subInput.isEmpty {
                    subpackets.append(decode(input: &subInput))
                }
            } else {
                let nSubpackets = consumeAsInt(bits: 11, input: &input)
                for _ in 0 ..< nSubpackets {
                    subpackets.append(decode(input: &input))
                }
            }
            return Packet(version: version, type: .op(op: opType, subpackets: subpackets))
        }
        fatalError()
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        var input = parseInput(rawInput)
        let packet = decode(input: &input)
        
        func versionSum(packet: Packet) -> Int {
            switch packet.type {
            case .op(_, let subpackets):
                return packet.version + subpackets.map(versionSum(packet:)).reduce(0, +)
            case .number:
                return packet.version
            }
        }
        
        return versionSum(packet: packet)
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        var input = parseInput(rawInput)
        let packet = decode(input: &input)
        
        func process(packet: Packet) -> Int {
            switch packet.type {
            case .number(let n):
                return n
            case .op(let op, let subpackets):
                let subValues = subpackets.map(process(packet:))
                switch op {
                case .sum:
                    return subValues.reduce(0, +)
                case .product:
                    return subValues.reduce(1, *)
                case .min:
                    return subValues.min()!
                case .max:
                    return subValues.max()!
                case .gt:
                    return subValues[0] > subValues[1] ? 1 : 0
                case .lt:
                    return subValues[0] < subValues[1] ? 1 : 0
                case .eq:
                    return subValues[0] == subValues[1] ? 1 : 0
                }
            }
        }
        
        return process(packet: packet)
    }
}
