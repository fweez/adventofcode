//
//  main.swift
//  day22
//
//  Created by Ryan Forsythe on 12/22/18.
//  Copyright © 2018 Zero Gravitas. All rights reserved.
//

import Foundation

enum RegionType: String {
    case Wet = "="
    case Rocky = "."
    case Narrow = "|"
    
    init(from: Int) {
        switch (from % 3) {
        case 0: self = .Rocky
        case 1: self = .Wet
        case 2: self = .Narrow
        default: self = .Rocky
        }
    }
    
    var risk: Int {
        switch self {
        case .Rocky: return 0
        case .Wet: return 1
        case .Narrow: return 2
        }
    }
    
    var compatible: [Tool] {
        switch self {
        case .Rocky: return [.Climb, .Torch]
        case .Wet: return [.Climb, .None]
        case .Narrow: return [.Torch, .None]
        }
    }
    
    var incompatible: Tool {
        switch self {
        case .Rocky: return .None
        case .Wet: return .Torch
        case .Narrow: return .Climb
        }
    }
}

enum Tool: CaseIterable {
    case Torch
    case Climb
    case None
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

class Region {
    var depth: Int = -1
    var target: Point = Point()
    var erosionLevels: [Point: Int] = [:]
    var cost: [Tool: [Point: Int]] = [:]
    
    init(depth: Int, target: Point) {
        self.depth = depth
        self.target = target
        
        let origin = Point(0,0)
        self.cost[.Torch] = [origin: 0]
        self.cost[.Climb] = [origin: 7]
        self.cost[.None] = [origin: 7]
    }
    
    func getRegion(_ point: Point) -> RegionType {
        return RegionType(from: self.getErosionLevel(point))
    }
    
    func getErosionLevel(_ point: Point) -> Int {
        if let value = self.erosionLevels[point] { return value }
        
        let x = point.x
        let y = point.y
        let geologicIndex: Int
        if x == 0 && y == 0 { geologicIndex = 0 }
        else if x == self.target.x && y == self.target.y { geologicIndex = 0 }
        else if y == 0 { geologicIndex = x * 16807 }
        else if x == 0 { geologicIndex = y * 48271 }
        else { geologicIndex = self.getErosionLevel(Point(x-1, y)) * self.getErosionLevel(Point(x, y-1)) }
        
        let erosionLevel = (geologicIndex + depth) % 20183
        self.erosionLevels[Point(x, y)] = erosionLevel
        return erosionLevel
    }
    
    var risk: Int {
        return (0...self.target.y).reduce(0) { (accum, y) -> Int in
            (0...self.target.x).reduce(accum, { $0 + self.getRegion(Point($1, y)).risk })
        }
    }
    
    func neighbors(_ point: Point) -> [Point] {
        var points: [Point] = []
        if point.y > 0 { points.append(Point(point.x, point.y-1))}
        if point.x > 0 { points.append(Point(point.x-1, point.y))}
        points.append(Point(point.x+1, point.y))
        points.append(Point(point.x, point.y+1))
        return points
    }
    
    func updateCosts(_ curr: Point, _ neighbor: Point) {
        let currRegion = self.getRegion(curr)
        let neighborRegion = self.getRegion(neighbor)
        self.cost[neighborRegion.incompatible]![neighbor] = Int.max
        for currTool in currRegion.compatible {
            for neighborTool in neighborRegion.compatible {
                let travelCost: Int
                if currTool == neighborTool { travelCost = 1 }
                else { travelCost = 8 }
                self.cost[neighborTool]![neighbor] = min(self.cost[neighborTool]![neighbor] ?? Int.max, self.cost[currTool]![curr]! + travelCost)
            }
        }
    }
    
    func findRoute(origin: Point, target: Point) {
        var open = [origin]
        while open.count > 0 {
            let curr = open.removeFirst()
            for neighbor in self.neighbors(curr) {
                if open.contains(neighbor) == false {
                    open.append(neighbor)
                }
                self.updateCosts(curr, neighbor)
            }
            open.sort { (a, b) -> Bool in
                let costA = self.cost[.Torch]?[a] ?? Int.max
                let costB = self.cost[.Torch]?[b] ?? Int.max
                if costA == costB {
                    let manhattanA = abs(target.x - a.x) - abs(target.y - a.y)
                    let manhattanB = abs(target.x - b.x) - abs(target.y - b.y)
                    return manhattanA < manhattanB
                } else {
                    return costA < costB
                }
            }
            if open.first == target {
                break
            }
        }
    }
}

let testRegion = Region(depth: 510, target: Point(10, 10))
print("Risk for test region: \(testRegion.risk)")
testRegion.findRoute(origin: Point(0,0), target: Point(10,10))

let region = Region(depth: 11991, target: Point(6, 797))
print("Risk for part a region: \(region.risk)")
