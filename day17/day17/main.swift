//
//  main.swift
//  day17
//
//  Created by Ryan Forsythe on 12/17/18.
//  Copyright © 2018 Zero Gravitas. All rights reserved.
//

import Foundation

struct Vein {
    var x: Range<Int>
    var y: Range<Int>
    
    init(specification: Substring) throws {
        let axisSpecifications = specification.split(separator: ",")
        if axisSpecifications.count != 2 { throw IngestError.SyntaxError }
        
        self.x = -1..<0
        self.y = -1..<0
        
        for axisSpec in axisSpecifications {
            let components = axisSpec.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: "=")
            if components.count != 2 { throw IngestError.SyntaxError }
            guard let rangeSpec = components.last?.split(separator: ".", maxSplits: 2, omittingEmptySubsequences: true) else {
                throw IngestError.SyntaxError
            }
            let axisRange: Range<Int>
            if rangeSpec.count == 1 {
                guard let a = Int(rangeSpec.first!) else { throw IngestError.SyntaxError }
                axisRange = a..<a + 1
            } else if rangeSpec.count == 2 {
                guard let a = Int(rangeSpec.first!),  let b = Int(rangeSpec.last!) else { throw IngestError.SyntaxError }
                axisRange = a..<b + 1
            } else {
                throw IngestError.SyntaxError
            }
            switch components.first! {
            case "x": self.x = axisRange
            case "y": self.y = axisRange
            default: throw IngestError.SyntaxError
            }
        }
        
        if self.x.count != 1 && self.y.count != 1 { throw IngestError.SyntaxError }
        if self.x == -1..<0 || self.y == -1..<0 { throw IngestError.SyntaxError }
    }
    
    var horizontal: Bool { return self.x.count > self.y.count }
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

enum MapElement {
    case Sand
    case Clay
    case Water
}
    
extension MapElement: CustomStringConvertible {
    var description: String {
        switch self {
        case .Sand: return " "
        case .Clay: return "#"
        case .Water: return "~"
        }
    }
}

extension MapElement: Equatable { }

class GroundScan {
    var walls: [Vein] = []
    var floors: [Vein] = []
    var map: [[MapElement]] = []
    
    // (re)set during load
    var scanRangeX = (Int.max, 0)
    var scanRangeY = (Int.max, 0)
    
    var open: [Point] = []
    var visited: Set<Point> = Set()
    
    func tick() -> Bool {
        if open.count == 0 { return false }
        let curr = open.removeFirst()
        assert(self.map[curr.y][curr.x] != .Clay)
        if self.visited.contains(curr) {
            // Don't revisit points
            return true
        }
        self.visited.insert(curr)
        
        let below = Point(curr.x, curr.y + 1)
        if below.y >= scanRangeY.1 {
            // Off the end of the map, we're done
            return true
        }
        
        if self.map[below.y][below.x] == .Sand {
            self.open.append(below)
            return true
        }
        
        // Below is clay or water. Search right and left for walls.
        var waterTransformPoints: Set<Point> = Set()
        var rightWall = false
        for x in curr.x..<self.map[curr.y].count {
            let rightward = Point(x, curr.y)
            if self.map[rightward.y][rightward.x] == .Clay {
                rightWall = true
                break
            }
            let belowRightward = Point(rightward.x, rightward.y + 1)
            if self.map[belowRightward.y][belowRightward.x] == .Sand {
                self.open.append(rightward)
                break
            } else {
                self.visited.insert(rightward)
            }
            waterTransformPoints.insert(rightward)
        }
        
        var leftWall = false
        var leftward = Point(curr.x, curr.y)
        while true {
            leftward = Point(leftward.x-1, leftward.y)
            if self.map[leftward.y][leftward.x] == .Clay {
                leftWall = true
                break
            }
            let belowLeftward = Point(leftward.x, leftward.y + 1)
            if self.map[belowLeftward.y][belowLeftward.x] == .Sand {
                self.open.append(leftward)
                break
            } else {
                self.visited.insert(leftward)
            }
            waterTransformPoints.insert(leftward)
        }
        
        if rightWall && leftWall {
            waterTransformPoints.insert(curr)
            for p in waterTransformPoints {
                if self.open.contains(p) {
                    let above = Point(p.x, p.y-1)
                    self.open.append(above)
                    self.visited.remove(above)
                }
                assert(self.map[p.y][p.x] == .Sand)
                self.map[p.y][p.x] = .Water
            }
            // curr isn't in open any more, so add it manually:
            let above = Point(curr.x, curr.y-1)
            self.open.append(above)
            self.visited.remove(above)
        }
        
        return true
    }
    
    var waterCount: Int {
        var count = 0
        for y in 1..<self.scanRangeY.1 {
            for x in 0..<(self.scanRangeX.1-self.scanRangeX.0) {
                let curr = Point(x, y)
                if self.visited.contains(curr) || self.map[curr.y][curr.x] == .Water {
                    count += 1
                }
            }
        }
        return count
    }
}

extension GroundScan: Ingester {
    convenience init?(inputFile: String) throws {
        self.init()
        if let filePath = Bundle.main.path(forResource: inputFile, ofType: "txt"),
            let input = try? String(contentsOfFile: filePath) {
            _ = try self.load(input)
        } else if let input = try? String(contentsOfFile: inputFile) {
            _ = try self.load(input)
        } else {
            print("Couldn't load file")
            return nil
        }
    }
    
    func load(_ input: String) throws {
        self.floors = []
        self.walls = []
        self.open = []
        self.visited = Set()
        
        for line in input.split(separator: "\n") {
            let vein = try Vein(specification: line)
            if vein.horizontal {
                self.floors.append(vein)
            } else {
                self.walls.append(vein)
            }
        }
        
        self.scanRangeX = (Int.max, 0)
        self.scanRangeY = (Int.max, 0)
        let getXlimits: ((Int, Int), Vein) -> (Int, Int) = { (min($1.x.lowerBound, $0.0), max($1.x.upperBound, $0.1)) }
        self.scanRangeX = self.walls.reduce(self.scanRangeX, getXlimits)
        self.scanRangeX = self.floors.reduce(self.scanRangeX, getXlimits)
        scanRangeX.0 = scanRangeX.0 - 1
        scanRangeX.1 = scanRangeX.1 + 1
        let getYLimits: ((Int, Int), Vein) -> (Int, Int) = { (min($1.y.lowerBound, $0.0), max($1.y.upperBound, $0.1)) }
        self.scanRangeY = self.walls.reduce(self.scanRangeY, getYLimits)
        self.scanRangeY = self.floors.reduce(self.scanRangeY, getYLimits)
        
        // NOTE: You're going from 0 to scanRangeY.1
        self.map = []
        for _ in 0..<self.scanRangeY.1 {
            self.map.append(Array(repeating: .Sand, count: self.scanRangeX.1 - self.scanRangeX.0))
        }
        
        for wall in self.walls {
            for y in wall.y {
                self.map[y][wall.x.lowerBound - self.scanRangeX.0] = .Clay
            }
        }
        for floor in self.floors {
            for x in floor.x {
                self.map[floor.y.lowerBound][x - self.scanRangeX.0] = .Clay
            }
        }
        
        let flowPoint = Point(500 - self.scanRangeX.0, 0)
        self.open = [flowPoint]
    }
}

extension GroundScan: CustomStringConvertible {
    var description: String {
        var textMap = ""
        
        for y in 0..<self.scanRangeY.1 {
            for x in 0..<(self.scanRangeX.1 - self.scanRangeX.0) {
                let curr = Point(x, y)
                let kind = self.map[curr.y][curr.x]
                switch kind {
                case .Sand:
                    if self.visited.contains(curr) {
                        textMap.append("|")
                    } else if self.open.first == curr {
                        textMap.append("F")
                    } else if self.open.contains(curr) {
                        textMap.append("o")
                    } else {
                        fallthrough
                    }
                case .Clay, .Water: textMap.append(kind.description)
                }
            }
            textMap.append("\n")
        }
        
        return textMap
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

let scanner = GroundScan()
do {
    try IngestManager().loadCommandLineFile(into: scanner)
    print("Loaded. Scan had \(scanner.floors.count) floors and \(scanner.walls.count) walls")
    print("Starting flow")
    while scanner.tick() {
        // do nothing
    }
    print("\(scanner.waterCount) water cells")
    print(scanner)
    
    let count = scanner.description.reduce(0, { (accum, c) -> Int in
        if c == "|" || c == "~" { return accum + 1 }
        else { return accum }
    })
    print("Double check: \(count) water cells")
    
} catch IngestError.NoFile {
    print("Running tests")
    
    func runMap(input: String, expectedWater: Int = -1) {
        do { try scanner.load(input) }
        catch { assertionFailure() }
        
        print("Initial open: \(scanner.open.count)")
        print("Next: \(scanner.open.first?.description ?? "n/a")")
        print(scanner)
        while scanner.tick() {
            print("Open: \(scanner.open.count)")
            print("Next: \(scanner.open.first?.description ?? "n/a")")
            print(scanner)
        }
        if expectedWater > 0 { assert(scanner.waterCount == expectedWater) }
    }
    
    runMap(input: """
    x=495, y=2..7
    y=7, x=495..501
    x=501, y=3..7
    x=498, y=2..4
    x=506, y=1..2
    x=498, y=10..13
    x=504, y=10..13
    y=13, x=498..504
    """, expectedWater: 57)
    runMap(input: """
    x=495, y=2..7
    y=7, x=495..505
    x=505, y=2..7
    y=2, x=495..505
    """)
    runMap(input: """
    x=498, y=2..6
    y=6, x=498..502
    x=502, y=2..6
    x=494, y=4..10
    y=10, x=494..506
    x=506, y=4..10
    
    """)
    runMap(input: """
    x=495, y=2..8
    y=8, x=495..505
    x=505, y=2..8
    x=498, y=4..6
    y=6, x=498..502
    x=502, y=4..6
    y=4, x=498..502
    """)
}
