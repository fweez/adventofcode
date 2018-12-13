//
//  main.swift
//  day13
//
//  Created by Ryan Forsythe on 12/13/18.
//  Copyright © 2018 Zero Gravitas. All rights reserved.
//

import Foundation

enum Turn {
    case Left, Straight, Right
}

enum Direction {
    case Up, Right, Down, Left
    
    func apply(turn: Turn) -> Direction {
        switch turn {
        case .Left:
            switch self {
            case .Up: return .Left
            case .Right: return .Up
            case .Down: return .Right
            case .Left: return .Down
            }
        case .Straight: return self
        case .Right:
            switch self {
            case .Up: return .Right
            case .Right: return .Down
            case .Down: return .Left
            case .Left: return .Up
            }
        }
    }
}

enum TrackPiece: Character {
    case UpDown = "|"
    case LeftRight = "-"
    case UpRightCorner = "/"
    case DownRightCorner = "\\"
    case Junction = "+"
    case NoTrack = " "
}

protocol MapElement {
    var locations: [Point: Character] { get set } // storing characters helps with parsing corners
}

struct Point {
    var x: Int
    var y: Int
    
    init(_ x: Int = -1, _ y: Int = -1) {
        self.x = x
        self.y = y
    }
    
    func apply(direction: Direction) -> Point {
        switch direction {
        case .Up: return Point(self.x, self.y - 1)
        case .Right: return Point(self.x + 1, self.y)
        case .Down: return Point(self.x, self.y + 1)
        case .Left: return Point(self.x - 1, self.y)
        }
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

class Cart: MapElement {
    static var lastIdentifier: Int = 0
    var identifier: Int
    var location: Point
    var direction: Direction
    var nextTurn: Turn = .Left
    
    // unused
    var locations: [Point: Character] = [:]
    
    init?(from c: Character, at point: Point) {
        Cart.lastIdentifier += 1
        self.identifier = Cart.lastIdentifier
        self.location = point
        switch c {
        case "^": self.direction = .Up
        case ">": self.direction = .Right
        case "v": self.direction = .Down
        case "<": self.direction = .Left
        default:
            assertionFailure("Illegal cart character")
            return nil
        }
    }
    
    var replacement: TrackPiece {
        switch self.direction {
        case .Up, .Down: return .UpDown
        case .Left, .Right: return .LeftRight
        }
    }
    
    func move(map: [[TrackPiece]]) {
        self.location = self.location.apply(direction: self.direction)
        switch map[self.location.y][self.location.x] {
        case .Junction:
            self.direction = self.direction.apply(turn: self.nextTurn)
            switch self.nextTurn {
            case .Left: self.nextTurn = .Straight
            case .Straight: self.nextTurn = .Right
            case .Right: self.nextTurn = .Left
            }
        case .UpRightCorner:
            switch self.direction {
            case .Up: self.direction = .Right
            case .Right: self.direction = .Up
            case .Down: self.direction = .Left
            case .Left: self.direction = .Down
            }
        case .DownRightCorner:
            switch self.direction {
            case .Up: self.direction = .Left
            case .Right: self.direction = .Down
            case .Down: self.direction = .Right
            case .Left: self.direction = .Up
            }
        case .UpDown:
            assert(self.direction == .Up || self.direction == .Down)
            break
        case .LeftRight:
            assert(self.direction == .Right || self.direction == .Left)
            break
        case .NoTrack:
            assertionFailure("Cart moved into empty space!")
        }
    }
    
    func crashed(carts: [Cart]) -> Bool {
        for cart in carts {
            if cart.identifier == self.identifier { continue }
            if cart.location == self.location { return true }
        }
        return false
    }
}

class Track {
    var carts: [Cart] = []
    var map: [[TrackPiece]] = []
    
    convenience init?(inputFile: String) {
        self.init()
        if let filePath = Bundle.main.path(forResource: inputFile, ofType: "txt"),
            let input = try? String(contentsOfFile: filePath) {
            self.load(input)
        } else if let input = try? String(contentsOfFile: inputFile) {
            self.load(input)
        } else {
            print("Couldn't load file")
            return nil
        }
    }
    
    var cartSorter: (Cart, Cart) -> Bool {
        return { (a: Cart, b: Cart) -> Bool in
            if a.location.y == b.location.y {
                return a.location.x < b.location.x
            } else {
                return a.location.y <= b.location.y
            }
        }
    }
    
    func load(_ input: String) {
        var newmap: [[TrackPiece]] = []
        var newcarts: [Cart] = []
        var curr: Point = Point(0, 0)
        newmap.append([])
        
        func parsingError(_ msg: String) {
            print("Parsing error at \(curr): \(msg)")
            assertionFailure()
        }
        
        for char in input {
            switch char {
            case "\n":
                curr = Point(0, curr.y+1)
                newmap.append([])
            case "-", "|","/","\\", "+", " ":
                guard let p = TrackPiece(rawValue: char) else {
                    parsingError("Illegal input?!")
                    return
                }
                newmap[curr.y].append(p)
                curr = Point(curr.x+1, curr.y)
            case "^", ">", "v", "<":
                guard let cart = Cart(from: char, at: curr) else {
                    parsingError("couldn't parse cart character")
                    return
                }
                newcarts.append(cart)
                newmap[curr.y].append(cart.replacement)
                curr = Point(curr.x+1, curr.y)
            default:
                parsingError("illegal map character \(char)")
                return
            }
        }
        newcarts.sort(by: self.cartSorter)
        self.carts = newcarts
        self.map = newmap
    }
    
    func tick() -> Point? {
        for cart in carts {
            cart.move(map: self.map)
            print(cart)
            if cart.crashed(carts: carts) {
                return cart.location
            }
        }
        print("Tick without crash")
        carts.sort(by: self.cartSorter)
        return nil
    }
}

extension Cart: CustomStringConvertible {
    var description: String {
        return "Cart \(self.identifier): \(self.location), facing \(self.direction)"
    }
}

if CommandLine.arguments.count > 0 && CommandLine.arguments.last?.suffix(3) == "txt" {
    let arg = CommandLine.arguments.last!
    guard let track = Track(inputFile: arg) else {
        print("Couldn't build track!")
        exit(1)
    }
    while true {
        if let crashPoint = track.tick() {
            print("Crashed at \(crashPoint)")
            break
        }
    }
} else {
    print("Running tests")
    let track = Track()
    track.load("""
    /-\\
    | |
    \\-/
    """)
    assert(track.map.count == 3, "Should have 3 rows in map")
    track.load("""
    /-\\ /-\\
    | | | |
    \\-/ \\-/
    """)
    assert(track.map.first?.count == 7, "should have 7 columns in map")
    track.load("""
        /-----\\
        |     |
        |  /--+--\\
        |  |  |  |
        \\--+--/  |
           |     |
           \\-----/
    """)
    assert(track.carts.count == 0, "should have 0 carts in map")
    track.load("""
    /-\\
    ^ |
    \\-/
    """)
    assert(track.carts.count == 1, "should have 1 carts in map")
    
    track.load("""
    /-\\
    ^ ^
    \\-/
    """)
    assert(track.carts.count == 2, "should have 1 carts in map")
    var crashed = false
    for _ in 0..<20 {
        if let crashPoint = track.tick() {
            crashed = true
            break
        }
    }
    assert(crashed, "carts should have crashed!")
}

// UNUSED, currently
class Path: MapElement {
    var intersections: (Intersection?, Intersection?) = (nil, nil)
    var locations: [Point: Character] = [:]
    var length: Int { return self.locations.count }
}

// UNUSED, currently
class Intersection: MapElement {
    /*  u
     |
     l-+-r
     |
     d
     */
    var paths: [Direction: Path] = [:]
    var locations: [Point: Character] = [:]
}

