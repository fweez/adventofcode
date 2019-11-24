import Foundation

enum Direction {
    case Up, Right, Down, Left
    
    var leftTurn: Direction {
        switch self {
        case .Up: return .Left
        case .Right: return .Up
        case .Down: return .Right
        case .Left: return .Down
        }
    }
    
    var rightTurn: Direction {
        switch self {
        case .Up: return .Right
        case .Right: return .Down
        case .Down: return .Left
        case .Left: return .Up
        }
    }
}

protocol MapElement {
    var locations: [Point: Character] { get set } // storing characters helps with parsing corners
}

class Point {
    var x: Int = -1
    var y: Int = -1
    
    convenience init(_ x: Int, _ y: Int) {
        self.init()
        self.x = x
        self.y = y
    }
    
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

class Path: MapElement {
    var intersections: (Intersection?, Intersection?) = (nil, nil)
    var locations: [Point: Character] = [:]
    var length: Int { return self.locations.count }
}

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

class Cart: MapElement {
    var currentLocation: Point = Point()
    var direction: Direction = .Up
    var locations: [Point: Character] = [:]
}

class Track {
    var carts: [Cart] = []
    
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
    
    func load(_ input: String) {
        var map: [[MapElement?]] = []
        var curr: Point = Point()
        map.append([])
        curr.x = 0
        curr.y = 0
        
        func parsingError(_ msg: String) {
            print("Parsing error at \(curr): \(msg)")
        }
        
        for char in input {
            var newelement: MapElement? = nil
            switch char {
            case "\n":
                curr.y += 1
                curr.x = -1 // will advance to 0 at end of switch
                map.append([])
                continue // avoid adding a new element after this switch
            case "-":
                guard let last = map[curr.y][curr.x-1] as? Path else {
                    parsingError("expected Path to left")
                    break
                }
                newelement = last
            case "|":
                guard let last = map[curr.y-1][curr.x] as? Path else {
                    parsingError("expected Path above")
                    break
                }
                newelement = last
            case "/":
                let left = Point(curr.x-1, curr.y)
                if curr.x > 0, let last = map[left.y][left.x] as? Path, last.locations[left] == "-" {
                    //  if there's a - on the left side, this is the lower right corner;
                    
                    newelement = last
                } else {
                    // if there isn't, this is the top right -- start a new Path
                    let p = Path()
                    newelement = p
                }
            case "\\":
                let left = Point(curr.x-1, curr.y)
                let above = Point(curr.x, curr.y-1)
                if curr.x > 0,
                    let last = map[left.y][left.x] as? Path,
                    last.locations[left] == "-" {
                    newelement = last
                } else if curr.y > 0,
                    let last = map[above.y][above.x] as? Path,
                    last.locations[above] == "|" {
                    newelement = last
                } else {
                    parsingError("Expected Path to left or above")
                }
            case "+" , "^", ">", "v", "<":
                // TODO
                parsingError("unimplemented character")
            case " ":
                // ignored
                break
            default:
                parsingError("illegal map character \(char)")
            }
            if var n = newelement {
                n.locations[curr] = char
            }
            map[curr.y][curr.x] = newelement
            curr.x += 1
        }
    }
    
    func findCrash() -> (Int, Int) {
        return (-1, -1)
    }
}

if CommandLine.arguments.count > 0 && CommandLine.arguments.last?.suffix(3) == "txt" {
    let arg = CommandLine.arguments.last!
    guard let track = Track(inputFile: arg) else {
        print("Couldn't build track!")
        exit(1)
    }
    print("First crash: \(track.findCrash())")
} else {
    let track1 = Track()
    track1.load("""
    /-\
    | |
    \\-/
    """)
    
}
