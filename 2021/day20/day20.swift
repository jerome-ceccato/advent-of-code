//
//  day20.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 18/12/2021.
//

import Foundation

final class Day20: AOCDay {
    struct Image {
        let enhancer: [Bool]
        let pixels: [[Bool]]
    }
    
    struct Pixel: Hashable, Equatable {
        let x: Int
        let y: Int
    }
    
    func parseInput(_ raw: String) -> Image {
        let chunks = raw.components(separatedBy: "\n\n").map { $0.trimmingCharacters(in: .whitespacesAndNewlines) }
        let enhancer = chunks[0].map { $0 == "#" }
        let pixelMap = chunks[1].components(separatedBy: "\n").map { $0.trimmingCharacters(in: .whitespaces) }
        let pixels = pixelMap.map { line in
            return line.map { $0 == "#" }
        }

        return Image(enhancer: enhancer, pixels: pixels)
    }
    
    func enhance(pixel: Pixel, input: [[Bool]], enhancer: [Bool], step: Int) -> Bool {
        var pixelValue = 0
        for yOffset in [-1, 0, 1] {
            for xOffset in [-1, 0, 1] {
                let target = Pixel(x: pixel.x + xOffset, y: pixel.y + yOffset)
                let inBounds = input.indices.contains(target.y) && input[target.y].indices.contains(target.x)
                let targetValue = inBounds ? input[target.y][target.x] : (step & 1 == 1)
                
                pixelValue = pixelValue << 1 + (targetValue ? 1 : 0)
            }
        }
        return enhancer[pixelValue]
    }
    
    func process(input: [[Bool]], enhancer: [Bool], step: Int) -> [[Bool]] {
        let growthRate = 1
        let ySize = input.count
        let xSize = input[0].count
        
        var output = [[Bool]]()
        for y in -growthRate ..< ySize + growthRate {
            var line = [Bool]()
            for x in -growthRate ..< xSize + growthRate {
                line.append(enhance(pixel: Pixel(x: x, y: y), input: input, enhancer: enhancer, step: step))
            }
            output.append(line)
        }
        return output
    }
    
    func display(pixels: [[Bool]]) -> String {
        var output = "\n"
        for y in 0 ..< pixels.count {
            for x in 0 ..< pixels[y].count {
                output += pixels[y][x] ? "#" : "."
            }
            output += "\n"
        }
        return output
    }
    
    func countActivePixels(pixels: [[Bool]]) -> Int {
        return pixels.reduce(0) { acc, line in
            return acc + line.reduce(0, { acc2, item in
                return acc2 + (item ? 1 : 0)
            })
        }
    }

    func part1(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        let processedInput = (0 ..< 2).reduce(input.pixels) { acc, step in
            return process(input: acc, enhancer: input.enhancer, step: step)
        }
        
        return countActivePixels(pixels: processedInput)
    }

    func part2(rawInput: String) -> CustomStringConvertible {
        let input = parseInput(rawInput)
        
        let processedInput = (0 ..< 50).reduce(input.pixels) { acc, step in
            return process(input: acc, enhancer: input.enhancer, step: step)
        }
        
        return countActivePixels(pixels: processedInput)
    }
}
