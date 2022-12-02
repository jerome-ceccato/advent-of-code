//
//  run.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 18/12/2021.
//

import Foundation

func run(day: Int) {
    let formattedDay = String(format: "%02d", day)
    
    guard let dayClass = Bundle.main.classNamed("adventofcode2021.Day\(formattedDay)") as? AOCDay.Type else {
        debugPrint("Day\(formattedDay) not found")
        return
    }
    guard let path = Bundle.main.path(forResource: "day\(formattedDay)", ofType: "txt") else {
        debugPrint("Could not find day\(formattedDay).txt")
        return
    }
    guard let input = try? String(contentsOfFile: path, encoding: .utf8) else {
        debugPrint("Could not read day\(formattedDay).txt")
        return
    }
    
    let puzzle = dayClass.init()

    print("Day\(formattedDay):")
    
    func formattedRunTime(_ date: Date) -> String {
        return String(format: "%.f", -date.timeIntervalSinceNow * 1000)
    }

    let part1StartDate = Date()
    let part1 = puzzle.part1(rawInput: input)
    print(" Part 1 (\(formattedRunTime(part1StartDate)) ms): \(part1)")
    
    let part2StartDate = Date()
    let part2 = puzzle.part2(rawInput: input)
    print(" Part 2 (\(formattedRunTime(part2StartDate)) ms): \(part2)")

    print("")
}
