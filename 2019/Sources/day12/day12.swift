import AOCShared
import Foundation
import Overture

public struct Vector3 {
    let x: Int
    let y: Int
    let z: Int
    
    public init(_ x: Int, _ y: Int, _ z: Int) {
        self.x = x
        self.y = y
        self.z = z
    }
    
    static let zero = Vector3(0, 0, 0)
    
    var energy: Int {
        return abs(x) + abs(y) + abs(z)
    }
}



extension Vector3: Hashable { }

extension Vector3: Equatable {
    public static func ==(lhs: Vector3, rhs: Vector3) -> Bool {
        lhs.x == rhs.x &&
            lhs.y == rhs.y &&
            lhs.z == rhs.z
    }
}

extension Vector3 {
    static func +(lhs: Vector3, rhs: Vector3) -> Vector3 {
        Vector3(lhs.x + rhs.x,
                lhs.y + rhs.y,
                lhs.z + rhs.z)
    }
    
    static func -(lhs: Vector3, rhs: Vector3) -> Vector3 {
        Vector3(rhs.x - lhs.x,
                rhs.y - lhs.y,
                rhs.z - lhs.z)
    }
}

let formatter: NumberFormatter = {
    let n = NumberFormatter()
    n.minimumIntegerDigits = 3
    n.positivePrefix = "+"
    return n
}()

extension Vector3: CustomStringConvertible {
    public var description: String {
        let fx = formatter.string(from: NSNumber(value: x))!
        let fy = formatter.string(from: NSNumber(value: y))!
        let fz = formatter.string(from: NSNumber(value: z))!
        return "<x=\(fx), y=\(fy), z=\(fz)>"
        
    }
}

public struct Moon {
    let position: Vector3
    let velocity: Vector3
}

extension Moon: CustomStringConvertible {
    public var description: String {
        "pos=\(position.description), vel=\(velocity.description)"
    }
}

extension Moon: Hashable { }

public typealias System = [Moon]

public func parse(_ fileName: String) -> System { parseFile(moonParser, fileName) }
public func part1(_ system: System) { print("Part 1: \(getTotalEnergy(system))") }
public func part2(_ system: System) { print("Part 2: \(findDuplicateSystem(system))") }

let moonParser: Parser<Moon, String> = zip(
    literal("<x="),
    intParser,
    literal(", y="),
    intParser,
    literal(", z="),
    intParser,
    literal(">"),
    with: { _, x, _, y, _, z, _ in Moon(position: Vector3(x, y, z), velocity: Vector3.zero) })

func getTotalEnergy(_ system: System) -> Int {
    calculateTotalEnergy((0..<1000)
        .reduce(system) { system, _ in
            updateSystem(system)
        })
}

func calculateTotalEnergy(_ system: System) -> Int {
    system
        .map { moon in moon.position.energy * moon.velocity.energy }
        .reduce(0, +)
}

var updateSystem: (System) -> System = pipe(applyGravity, applyVelocity)

func applyGravity(_ system: System) -> System {
    system.map { moon -> Moon in
        system.reduce(moon) { this, other in
            Moon(position: this.position,
                 velocity: this.velocity + normal(moon.position - other.position))
        }
    }
}

func normal(_ v: Vector3) -> Vector3 {
    Vector3(normal(v.x), normal(v.y), normal(v.z))
}

func normal(_ i: Int) -> Int {
    if i < 0 { return -1 }
    if i == 0 {  return 0 }
    return 1
}

func applyVelocity(_ system: System) -> System {
    system.map {
        Moon(position: $0.position + $0.velocity,
             velocity: $0.velocity)
    }
}

func findDuplicateSystem(_ system: System) -> Int {
    var seenPositions: [Vector3: (iteration: Int, moonIdx: Int, moon: Moon)] = [:]
    return calculateTotalEnergy((1...10000)
        .reduce(system) { system, iteration in
            let s = updateSystem(system)
            //print(iteration)
            s
                .enumerated()
                .forEach {
                    //print("\($0): \($1)")
                    if let info = seenPositions[$1.position] {
                        print(iteration)
                        print($1)
                        print("Saw this at iteration \(info.iteration), moon \(info.moonIdx): \(info.moon)")
                    }
                    seenPositions[$1.position] = (iteration, $0, $1)
                }
            return s
        })
}


