//
//  main.swift
//  day18
//
//  Created by Ryan Forsythe on 12/18/18.
//  Copyright © 2018 Zero Gravitas. All rights reserved.
//

import Foundation

enum AcreType: Character {
    case Open = "."
    case Tree = "|"
    case Yard = "#"
}

struct Point {
    var x: Int
    var y: Int
    
    init(_ x: Int = -1, _ y: Int = -1) {
        self.x = x
        self.y = y
    }
}

extension Point: CustomStringConvertible {
    var description: String { return "\(self.x, self.y)" }
}

extension Point: Equatable {
    static func ==(lhs: Point, rhs: Point) -> Bool {
        return lhs.x == rhs.x && lhs.y == rhs.y
    }
}

extension Point: Hashable {
    func hash(into hasher: inout Hasher) {
        hasher.combine(x)
        hasher.combine(y)
    }
}

class CollectionArea: Ingester {
    var map: [[AcreType]] = []
    func load(_ input: String) throws {
        var newMap: [[AcreType]] = []
        for line in input.split(separator: "\n") {
            newMap.append(try line.map { (c) -> AcreType in
                guard let type = AcreType(rawValue: c) else {
                    throw IngestError.SyntaxError
                }
                return type
            })
        }
        self.map = newMap
    }
    
    func tick() {
        var newMap: [[AcreType]] = []
        for (y, row) in self.map.enumerated() {
            var newRow: [AcreType] = []
            for (x, type) in row.enumerated() {
                let location = Point(x, y)
                switch type {
                case .Open:
                    if self.countAdjacentTrees(location) >= 3 {
                        newRow.append(.Tree)
                    } else {
                        newRow.append(type)
                    }
                case .Tree:
                    if self.countAdjacentYards(location) >= 3 {
                        newRow.append(.Yard)
                    } else {
                        newRow.append(type)
                    }
                case .Yard:
                    if self.countAdjacentYards(location) >= 1 && self.countAdjacentTrees(location) >= 1 {
                        newRow.append(type)
                    } else {
                        newRow.append(.Open)
                    }
                }
            }
            newMap.append(newRow)
        }
        self.map = newMap
    }
    
    func countAdjacentTrees(_ location: Point) -> Int {
        return self.neighbors(location).reduce(0, { self.map[$1.y][$1.x] == .Tree ? $0 + 1 : $0 })
    }
    
    func countAdjacentYards(_ location: Point) -> Int {
        return self.neighbors(location).reduce(0, { self.map[$1.y][$1.x] == .Yard ? $0 + 1 : $0 })
    }
    
    func neighbors(_ location: Point) -> [Point] {
        var neighbors: [Point] = []
        if location.x > 0 {
            if location.y > 0 {
                neighbors.append(Point(location.x-1, location.y-1))
            }
            neighbors.append(Point(location.x-1, location.y))
            if location.y < self.map.count - 1 {
                neighbors.append(Point(location.x-1, location.y+1))
            }
        }
        if location.y > 0 {
            neighbors.append(Point(location.x, location.y-1))
            if location.x < self.map.first!.count - 1 {
                neighbors.append(Point(location.x+1, location.y-1))
            }
        }
        if location.y < self.map.count - 1 {
            neighbors.append(Point(location.x, location.y+1))
            if location.x < self.map.first!.count - 1 {
                neighbors.append(Point(location.x+1, location.y+1))
            }
        }
        if location.x < self.map.first!.count - 1 {
            neighbors.append(Point(location.x+1, location.y))
        }
        
        return neighbors
    }
    
    var yardCount: Int {
        return self.map.reduce(0, { (accum, row) -> Int in
            return row.reduce(accum, { (ra, t) -> Int in
                if t == .Yard { return ra + 1 }
                else { return ra }
            })
        })
    }
    
    var treeCount: Int {
        return self.map.reduce(0, { (accum, row) -> Int in
            return row.reduce(accum, { (ra, t) -> Int in
                if t == .Tree { return ra + 1 }
                else { return ra }
            })
        })
    }
    
    var value: Int { return self.yardCount * self.treeCount  }
}

extension CollectionArea: CustomStringConvertible {
    var description: String {
        return self.map.map({ (row) -> String in
            row.map({ (type) -> String in
                String(type.rawValue)
            }).joined()
        }).joined(separator: "\n")
    }
}

protocol Ingester {
    func load(_ input: String) throws
}

enum IngestError: Error {
    case NoFile
    case FileNotLoaded
    case SyntaxError
}

class IngestManager {
    func loadCommandLineFile(into consumer: Ingester) throws {
        if CommandLine.arguments.count > 0 && CommandLine.arguments.last?.suffix(3) == "txt" {
            let inputFile = CommandLine.arguments.last!
            try self.loadFile(named: inputFile, into: consumer)
        } else {
            throw IngestError.NoFile
        }
    }
    
    func loadFile(named inputFile: String, into consumer: Ingester) throws {
        if let filePath = Bundle.main.path(forResource: inputFile, ofType: "txt"),
            let input = try? String(contentsOfFile: filePath) {
            try consumer.load(input)
        } else if let input = try? String(contentsOfFile: inputFile) {
            try consumer.load(input)
        } else {
            throw IngestError.FileNotLoaded
        }
    }
}

let area = CollectionArea()
do {
    try IngestManager().loadCommandLineFile(into: area)
    print("Initial state")
    print(area)
    for i in 0..<10 {
        print("minute \(i+1)")
        area.tick()
        print(area)
    }
    print("Value: \(area.value)")
} catch IngestError.NoFile {
    try area.load("""
    .#.#...|#.
    .....#|##|
    .|..|...#.
    ..|#.....#
    #.#|||#|#|
    ...#.||...
    .|....|...
    ||...#|.#|
    |.||||..|.
    ...#.|..|.
    """)
    print("Initial state")
    print(area)
    for i in 0..<10 {
        print("minute \(i+1)")
        area.tick()
        print(area)
    }
    print("Value: \(area.value)")
}
