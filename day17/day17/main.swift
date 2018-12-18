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

enum FlowDirection {
    case Right, Down, Left, Split
}

extension FlowDirection: CustomStringConvertible {
    var description: String {
        switch self {
        case .Right: return ">"
        case .Down: return "v"
        case .Left: return "<"
        case .Split: return "+"
        }
    }
}

enum MapElement {
    case Sand
    case Clay
    case FlowDown
    case FlowLeft
    case FlowRight
    case FlowSplit
    case FlowStopped
    case Water
}
    
extension MapElement: CustomStringConvertible {
    var description: String {
        switch self {
        case .Sand: return "."
        case .Clay: return "#"
        case .Water: return "~"
        case .FlowDown: return "v"
        case .FlowLeft: return "<"
        case .FlowRight: return ">"
        case .FlowSplit: return "+"
        case .FlowStopped: return "x"
        }
    }
}

extension MapElement: Equatable { }

class GroundScan {
    var walls: [Vein] = []
    var floors: [Vein] = []
    var map: [[MapElement]] = []
    var flow: [Point] = []
    
    // (re)set during load
    var scanRangeX = (Int.max, 0)
    var scanRangeY = (Int.max, 0)
    
    func tick() {
        if flow.count == 0 { return }
        let point = flow.removeLast()
        if point.y == self.scanRangeY.1 - 1 { return }
        switch self.map[point.y][point.x] {
        case .FlowDown: self.flow.append(contentsOf: self.flowDown(point))
        case .FlowRight: self.flow.append(contentsOf: self.flowRight(point))
        case .FlowLeft: self.flow.append(contentsOf: self.flowLeft(point))
        case .FlowSplit: self.flow.append(contentsOf: self.flowSplit(point))
        default: break
        }
    }
    
    func flowDown(_ point: Point) -> [Point] {
        let next = Point(point.x, point.y+1)
        switch self.map[next.y][next.x] {
        case .Clay, .Water, .FlowStopped:
            self.map[point.y][point.x] = .FlowSplit
            return [point]
        case .Sand:
            self.map[next.y][next.x] = .FlowDown
            return [point, next]
        case .FlowDown, .FlowLeft, .FlowRight, .FlowSplit:
            return []
        }
    }
    
    func flowRight(_ point: Point) -> [Point] {
        return self.flowHorizontally(curr: point, next: Point(point.x + 1, point.y), replacement: .FlowRight)
    }
    
    func flowLeft(_ point: Point) -> [Point] {
        return self.flowHorizontally(curr: point, next: Point(point.x - 1, point.y), replacement: .FlowLeft)
    }
    
    func flowHorizontally(curr: Point, next: Point, replacement: MapElement) -> [Point] {
        let below = Point(curr.x, curr.y + 1)
        switch self.map[below.y][below.x] {
        case .Sand:
            self.map[curr.y][curr.x] = .FlowDown
            return [curr]
        case .FlowRight, .FlowLeft, .FlowDown, .FlowSplit, .FlowStopped:
            return []
        default: break
        }
        
        if next.y < 0 || next.y > self.map.count || next.x < 0 || next.x > self.map.first!.count {
            print(self)
            print("Flowed too far horizontally! Next is \(next). Below is a '\(self.map[below.y][below.x])', == sand \(self.map[below.y][below.x] == .Sand)")
            assertionFailure()
        }
        
        switch self.map[next.y][next.x] {
        case .Clay:
            self.map[curr.y][curr.x] = .FlowStopped
        case .Water:
            self.map[curr.y][curr.x] = .Water
        case .Sand:
            self.map[next.y][next.x] = replacement
            return [curr, next]
        case .FlowStopped:
            self.map[curr.y][curr.x] = .FlowStopped
        case .FlowDown, .FlowLeft, .FlowRight, .FlowSplit: break
        }
        return []
    }
    
    func flowSplit(_ point: Point) -> [Point] {
        var out: [Point] = []
        let left = Point(point.x - 1, point.y)
        let right = Point(point.x + 1, point.y)
        
        if self.map[left.y][left.x] == .Clay && self.map[right.y][right.x] == .Clay {
            self.map[point.y][point.x] = .Water
            return []
        }
        
        var leftStopped = false
        switch self.map[left.y][left.x] {
        case .Sand:
            self.map[left.y][left.x] = .FlowLeft
            out.append(left)
        case .Water:
            self.map[point.y][point.x] = .Water
        case .Clay, .FlowStopped:
            leftStopped = true
        default: break
        }
        
        var rightStopped = false
        switch self.map[right.y][right.x] {
        case .Sand:
            self.map[right.y][right.x] = .FlowRight
            out.append(right)
        case .Water:
            self.map[point.y][point.x] = .Water
        case .Clay, .FlowStopped:
            rightStopped = true
        default: break
        }
        
        if leftStopped && rightStopped {
            self.map[point.y][point.x] = .Water
            out.append(left)
            out.append(right)
        }
        
        if out.count > 0 {
            out.insert(point, at: 0)
        }
        
        return out
    }
    
    var waterCounts: (Int, Int) {
        var water = 0
        var flow = 0
        for row in self.map[self.scanRangeY.0..<self.scanRangeY.1] {
            for element in row {
                switch element {
                case .Clay, .Sand: break
                case .Water: water += 1
                case .FlowLeft, .FlowRight, .FlowDown, .FlowSplit, .FlowStopped: flow += 1
                }
            }
        }
        return (water, flow)
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
        self.map[flowPoint.y][flowPoint.x] = .FlowDown
        self.flow = [flowPoint]
    }
}

extension GroundScan: CustomStringConvertible {
    var description: String {
        var textMap = self.map.map({ $0.map( { $0.description }) })
        
        if self.flow.count > 0 {
            for p in self.flow {
                textMap[p.y][p.x] = "f"
            }
            let lastFlow = self.flow.last!
            textMap[lastFlow.y][lastFlow.x] = "F"
        }
        
        return textMap.map({ $0.joined() }).joined(separator: "\n")
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
    while scanner.flow.count > 0 {
        scanner.tick()
    }
    let waterCounts = scanner.waterCounts
    print("\(waterCounts.0) stream cells, \(waterCounts.1) pool cells = \(waterCounts.0 + waterCounts.1) water cells")
    print(scanner)
} catch IngestError.NoFile {
    print("Running tests")
    try? scanner.load("""
    x=495, y=2..7
    y=7, x=495..501
    x=501, y=3..7
    x=498, y=2..4
    x=506, y=1..2
    x=498, y=10..13
    x=504, y=10..13
    y=13, x=498..504
    """)
    print("Loaded. Scan had \(scanner.floors.count) floors and \(scanner.walls.count) walls")
    print(scanner)
    
    while scanner.flow.count > 0 {
        print("Flow remaining: \(scanner.flow.count)")
        scanner.tick()
        print(scanner)
    }
    let lastWaterCount = scanner.waterCounts
    assert(lastWaterCount.0 + lastWaterCount.1 == 57)
    
    try? scanner.load("""
    x=495, y=2..7
    y=7, x=495..505
    x=505, y=2..7
    y=2, x=495..505
    """)
    print(scanner)
    while scanner.flow.count > 0 {
        print("Flow remaining: \(scanner.flow.count)")
        scanner.tick()
        print(scanner)
    }
    
    do {
        try scanner.load("""
        x=498, y=2..6
        y=6, x=498..502
        x=502, y=2..6
        x=494, y=4..10
        y=10, x=494..506
        x=506, y=4..10
        
        """)
    } catch {
        assertionFailure()
    }
    print(scanner)
    while scanner.flow.count > 0 {
        print("Flow remaining: \(scanner.flow.count)")
        scanner.tick()
        print(scanner)
    }
}
