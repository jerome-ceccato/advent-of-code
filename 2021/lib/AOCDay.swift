//
//  AOCDay.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 06/12/2021.
//
//  Based on https://github.com/nuudles/advent-of-code-swift-template

import Foundation

protocol AOCDay: AnyObject {
    init()

    func part1(rawInput: String) -> CustomStringConvertible
    func part2(rawInput: String) -> CustomStringConvertible
}
