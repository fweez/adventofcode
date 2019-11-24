//
//  main.swift
//  day15
//
//  Created by Ryan Forsythe on 12/15/18.
//  Copyright © 2018 Zero Gravitas. All rights reserved.
//

import Foundation

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

enum ElementKind: Character {
    case Wall = "#"
    case Space = "."
    case Elf = "E"
    case Goblin = "G"
}

class MapElement {
    let kind: ElementKind
    let attackPower = 3
    let originalLocation: Point
    
    var hitPoints = 200 {
        willSet {
            switch self.kind {
            case .Wall, .Space: assertionFailure()
            default: break
            }
        }
    }
    
    var location: Point {
        willSet {
            switch self.kind {
            case .Wall, .Space: assertionFailure()
            default: break
            }
        }
    }
    
    init?(_ character: Character, location: Point) {
        guard let k = ElementKind(rawValue: character) else { return nil }
        self.kind = k
        self.location = location
        self.originalLocation = location
    }
    
    func attackable(other: MapElement) -> Bool {
        switch self.kind {
        case .Wall, .Space: return false
        case .Goblin, .Elf:
            switch other.kind {
            case .Wall, .Space: return false
            case .Goblin, .Elf: return self.kind != other.kind
            }
        }
    }
    
    func attack(other: MapElement) {
        switch self.kind {
        case .Wall, .Space: assertionFailure()
        case .Goblin, .Elf:
            other.hitPoints -= self.attackPower
        }
    }
    
    static func space(location: Point) -> MapElement {
        return MapElement(".", location: location)!
    }
}

extension MapElement: CustomStringConvertible {
    var description: String {
        return "\(self.kind.rawValue)(\(self.hitPoints))"
    }
}

extension MapElement: Equatable {
    static func ==(lhs: MapElement, rhs: MapElement) -> Bool {
        return lhs.kind == rhs.kind && lhs.originalLocation == rhs.originalLocation
    }
}

extension MapElement: Hashable {
    func hash(into hasher: inout Hasher) {
        hasher.combine(self.kind.rawValue)
        hasher.combine(self.location)
    }
}

enum ParseError: Error {
    case IllegalCharacter(character: Character, location: Point)
}

enum ActorError: Error {
    case IllegalActor(actor: MapElement)
}

class Map: CustomStringConvertible {
    var map: [[MapElement]] = []
    
    convenience init?(inputFile: String) throws {
        self.init()
        if let filePath = Bundle.main.path(forResource: inputFile, ofType: "txt"),
            let input = try? String(contentsOfFile: filePath) {
            try self.load(input)
        } else if let input = try? String(contentsOfFile: inputFile) {
            try self.load(input)
        } else {
            print("Couldn't load file")
            return nil
        }
    }
    
    func load(_ input: String) throws {
        var newmap: [[MapElement]] = [[]]
        var curr = Point(0,0)
        
        for character in input {
            if character == "\n" {
                newmap.append([])
                curr.y += 1
                curr.x = 0
                continue
            }
            guard let element = MapElement(character, location: curr) else {
                throw ParseError.IllegalCharacter(character: character, location: curr)
            }
            newmap[curr.y].append(element)
            curr.x += 1
        }
        self.map = newmap
    }
    
    func run(printMap: Bool = false) -> Int {
        var i = 0
        while true {
            if self.roundCompleted() {
                i += 1
                if printMap {
                    print("Round \(i) complete")
                }
            } else if printMap {
                print("Round \(i+1) incomplete")
            }
            if printMap {
                print(self)
            }
            if self.checkVictory() {
                break
            }
        }
        
        var goblinHP = 0
        var elfHP = 0
        for row in self.map {
            for element in row {
                switch element.kind {
                case .Goblin: goblinHP += element.hitPoints
                case .Elf: elfHP += element.hitPoints
                default: break
                }
            }
        }
        print("Combat ends after \(i) rounds")
        print("Goblins had \(goblinHP) total hp")
        print("Elves had \(elfHP) total hp")
        let outcome = i * max(elfHP, goblinHP)
        print("Outcome: \(outcome)")
        return outcome
    }
    
    func checkVictory() -> Bool {
        var elfcount = 0
        var gobcount = 0
        for row in self.map {
            for element in row {
                switch element.kind {
                case .Wall, .Space: continue
                case .Goblin: gobcount += 1
                case .Elf: elfcount += 1
                }
            }
        }
        return elfcount == 0 || gobcount == 0
    }
    
    var description: String {
        var out = ""
        for row in self.map {
            for col in row {
                out.append(col.kind.rawValue)
            }
            out.append("   ")
            out.append(row.filter({$0.kind == .Goblin || $0.kind == .Elf}).map({ "\($0)" }).joined(separator: ", "))
            out.append("\n")
        }
        return out
    }
    
    func targets(actor: MapElement) throws -> [MapElement] {
        var targets: [MapElement] = []
        for row in self.map {
            for element in row {
                if actor.attackable(other: element) {
                    targets.append(element)
                }
            }
        }
        return targets
    }
    
    func roundCompleted() -> Bool {
        var expectedActors: [MapElement] = []
        for row in self.map {
            for actor in row {
                switch actor.kind {
                case .Goblin, .Elf: expectedActors.append(actor)
                default: break
                }
            }
        }
        
        
        var acted: [MapElement] = []
        for var actor in expectedActors {
            print("Currently performing turn: \(actor) at \(actor.location)")
            if actor.hitPoints > 0 {
                if self.performTurn(actor: &actor) == false {
                    print("No more enemies available, bailing out")
                    break
                }
                print("Performed turn: \(actor) at \(actor.location)")
            }
            acted.append(actor)
        }
        
        let unActed = expectedActors.filter({ $0.hitPoints > 0 && !acted.contains($0) })
        print("Unacted: \(unActed)")
        return unActed.isEmpty
    }
    
    func performTurn(actor: inout MapElement) -> Bool {
        var enemyFound = false
        for row in self.map {
            for element in row {
                if actor.attackable(other: element) {
                    enemyFound = true
                    break
                }
            }
            if enemyFound {
                break
            }
        }
        if enemyFound == false { return false }
        
        let inRangePositions = self.findAllInRangePositions(attackableBy: actor)
        if inRangePositions.count == 0 {
            // either a bad map or no targets -- let's assume the latter
            return true
            //assertionFailure("Bad map -- \(actor) at \(actor.location) can't reach any of its targets")
        }
        
        let costs = self.calculateReachableCosts(origin: actor)
        
        /*
        var debugMap = ""
        for row in self.map {
            for element in row {
                switch element.kind {
                case .Wall, .Goblin, .Elf: debugMap.append(element.kind.rawValue)
                case .Space:
                    if let cost = costs[element] { debugMap.append("\(cost)") }
                    else { debugMap.append("*") }
                }
            }
            debugMap.append("\n")
        }
        print("Path Costs:")
        print(debugMap)
        */
        
        // Find lowest-cost reachable in range to a target
        let costedInRange = inRangePositions.keys.sorted { (a, b) -> Bool in
            return costs[a] ?? Int.max < costs[b] ?? Int.max
        }
        let closestScore = costs[costedInRange.first!] ?? 0
        // If we need to take a step, do so
        if closestScore > 0 {
            let allClosest = inRangePositions.keys.filter( { costs[$0] ?? Int.max == closestScore })
            var firstSteps = allClosest.map({ self.findFirstStep(from: actor, to: $0, costs: costs) })
            firstSteps.sort { (a, b) -> Bool in
                if a.location.y == b.location.y { return a.location.x < b.location.x }
                else { return a.location.y < b.location.y }
            }
            let firstStepElement = firstSteps.first!
            self.map[actor.location.y][actor.location.x] = MapElement.space(location: actor.location)
            self.map[firstStepElement.location.y][firstStepElement.location.x] = actor
            actor.location = firstStepElement.location
            
        }
        
        // If we're at an in range, attack
        var target: MapElement?
        for other in self.allNeighbors(for: actor) {
            if actor.attackable(other: other) && (target?.hitPoints ?? Int.max) > other.hitPoints {
                target = other
            }
        }
        if let target = target {
            actor.attack(other: target)
            if target.hitPoints <= 0 {
                print("\(actor) killed \(target) to death (by stabbing)")
                self.map[target.location.y][target.location.x] = MapElement.space(location: target.location)
            }
        }
        
        return true
    }
    
    // Look at every element in the map and generate a map of in-range elements to what they hit
    func findAllInRangePositions(attackableBy actor: MapElement) -> [MapElement: MapElement] {
        var inRangePositions: [MapElement: MapElement] = [:]
        for row in self.map {
            for target in row {
                // ... for attackable elements
                guard actor.attackable(other: target) else {
                    continue
                }
                // ... and find their in-range positions
                for inRange in self.allNeighbors(for: target) {
                    // if this point has already been added, that means an "earlier" (ie, higher-priority) target had been found
                    if (inRange == actor || inRange.kind == .Space) && inRangePositions[inRange] == nil {
                        inRangePositions[inRange] = target
                    }
                }
            }
        }
        return inRangePositions
    }
    
    func calculateReachableCosts(origin: MapElement) -> [MapElement: Int] {
        // Calculate costs to all reachable locations
        var costs: [MapElement: Int] = [origin: 0]
        var unvisited: [MapElement] = [origin]
        while unvisited.count > 0 {
            let curr = unvisited.removeFirst()
            let cost = costs[curr]!
            for location in self.walkableNeighbors(for: curr) {
                if let currentNodeCost = costs[location] {
                    costs[location] = min(cost + 1, currentNodeCost)
                } else {
                    costs[location] = cost + 1
                    if !unvisited.contains(location) { unvisited.append(location) }
                }
            }
        }
        return costs
    }
    
    func findFirstStep(from origin: MapElement, to target: MapElement, costs: [MapElement: Int]) -> MapElement {
        var path: [MapElement] = [target]
        while true {
            let curr = path.last!
            // Is the origin next to us? We're done
            if self.allNeighbors(for: curr).filter({ $0 == origin }).first != nil { break }
            // Otherwise, find the walkable neighbors which aren't in our path, then sort by cost
            var neighbors = self.walkableNeighbors(for: curr).filter( { !path.contains($0) })
            neighbors.sort { costs[$0] ?? Int.max < costs[$1] ?? Int.max }
            // and make the next step the lowest-cost one
            if let next = neighbors.first {
                path.append(next)
            }
        }
        return path.last!
    }
    
    func allNeighbors(for actor: MapElement) -> [MapElement] {
        var positions: [MapElement] = []
        if actor.location.y > 0 {
            positions.append(self.map[actor.location.y-1][actor.location.x])
        }
        if actor.location.x > 0 {
            positions.append(self.map[actor.location.y][actor.location.x-1])
        }
        if actor.location.x < self.map[actor.location.y].count - 1 {
            positions.append(self.map[actor.location.y][actor.location.x+1])
        }
        if actor.location.y < self.map.count - 1 {
            positions.append(self.map[actor.location.y+1][actor.location.x])
        }
        return positions
    }
    
    func walkableNeighbors(for actor: MapElement) -> [MapElement] {
        return self.allNeighbors(for: actor).filter( { $0.kind == .Space })
    }
}

if CommandLine.arguments.count > 0 && CommandLine.arguments.last?.suffix(3) == "txt" {
    guard let m = try Map(inputFile: CommandLine.arguments.last!) else {
        print("Couldn't load input file")
        exit(1)
    }
    print(m)
    m.run(printMap: true)
} else {
    print("Running tests")
    
    let m = Map()
    /*
    """
    ####
    #E.#
    #.G#
    ####
    """
    """
    #########
    #G..G..G#
    #.......#
    #.......#
    #G..E..G#
    #.......#
    #.......#
    #G..G..G#
    #########
    """
    
     */
    let tests: [(String, Int)] = [
    ("""
    #######
    #.G...#
    #...EG#
    #.#.#G#
    #..G#E#
    #.....#
    #######
    """, 27730),
    ("""
    #######
    #G..#E#
    #E#E.E#
    #G.##.#
    #...#E#
    #...E.#
    #######
    """, 36334),
    ("""
    #######
    #E..EG#
    #.#G.E#
    #E.##E#
    #G..#.#
    #..E#.#
    #######
    """, 39514),
    ("""
    #######
    #E.G#.#
    #.#G..#
    #G.#.G#
    #G..#.#
    #...E.#
    #######
    """, 27755),
    ("""
    #######
    #.E...#
    #.#..G#
    #.###.#
    #E#G#G#
    #...#G#
    #######
    """, 28944),
    ("""
    #########
    #G......#
    #.E.#...#
    #..##..G#
    #...##..#
    #...#...#
    #.G...G.#
    #.....G.#
    #########
    """, 18740)]
    for (idx, (input, score)) in tests.enumerated() {
        try m.load(input)
        assert(m.run(printMap: true) == score, "Error in test \(idx)")
    }
    
    try m.load("""
    #####
    #GG##
    #.###
    #..E#
    #.#G#
    #.E##
    #####
    """)
    m.run(printMap: true)
    
    try m.load("""
    ####
    ##E#
    #GG#
    ####
    """)
    m.run(printMap: true)
    
    try m.load("""
    #######
    #.E..G#
    #.....#
    #G....#
    #######
    """)
    m.roundCompleted()
    print(m)
}
