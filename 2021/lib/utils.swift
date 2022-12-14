//
//  utils.swift
//  adventofcode2021
//
//  Created by Jerome Ceccato on 18/12/2021.
//

import Foundation

public extension StringProtocol {
    subscript(offset: Int) -> Character {
        self[index(startIndex, offsetBy: offset)]
    }
}

public extension StringProtocol {
    func index(of char: Character) -> Int? {
        return firstIndex(of: char)?.utf16Offset(in: self)
      }
}

public extension Collection {
    subscript (safe offset: Int) -> Element? {
        let index = index(startIndex, offsetBy: offset)
        return indices.contains(index) ? self[index] : nil
    }
}
