//
//  main.swift
//  day22
//
//  Created by Ryan Forsythe on 12/22/18.
//  Copyright © 2018 Zero Gravitas. All rights reserved.
//

import Foundation

func generateRegion(depth: Int, targetX: Int, targetY: Int) -> [[Int]] {
    var erosionLevels = Array(repeating: Array(repeating: -1, count: targetX + 1), count: targetY + 1)
    
    for y in 0...targetY {
        for x in 0...targetX {
            let geologicIndex: Int
            
            if x == 0 && y == 0 { geologicIndex = 0 }
            else if x == targetX && y == targetY { geologicIndex = 0 }
            else if y == 0 { geologicIndex = x * 16807 }
            else if x == 0 { geologicIndex = y * 48271 }
            else { geologicIndex = erosionLevels[y][x-1] * erosionLevels[y-1][x] }
            
            erosionLevels[y][x] = (geologicIndex + depth) % 20183
        }
    }
    
    return erosionLevels
}

enum RegionTypes: String {
    case Wet = "="
    case Rocky = "."
    case Narrow = "|"
    
    init?(from: Int) {
        switch from {
        case 0: self = .Rocky
        case 1: self = .Wet
        case 2: self = .Narrow
        default: return nil
        }
    }
}

func map(region: [[Int]]) -> String {
    return region.map({ (row) -> String in
        row.map( { RegionTypes(from: $0 % 3)?.rawValue ?? "X" }).joined()
    }).joined(separator: "\n")
}

func risk(region: [[Int]]) -> Int {
    return region.reduce(0, { (accum, row) -> Int in
        return row.reduce(accum, { $0 + ($1 % 3) })
    })
}

let testRegion = generateRegion(depth: 510, targetX: 10, targetY: 10)
print(map(region: testRegion))
print("Risk for test region: \(risk(region: testRegion))")

let region = generateRegion(depth: 11991, targetX: 6, targetY: 797)
print("Risk for part a region: \(risk(region: region))")
