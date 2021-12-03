import Foundation
import Overture
import Parsing

public protocol DayProtocol {
    associatedtype T
    func parse(input: String) -> T
    func part1(input: T) -> Void
    func part2(input: T) -> Void
}

public func time(_ f: () -> Void) -> String {
    let nf = NumberFormatter()
    nf.maximumFractionDigits = 5
    let start = Date()
    f()
    let elapsed = Date().timeIntervalSince(start)
    return nf.string(for: elapsed) ?? "Couldn't format time \(elapsed)"
}

public func run<A>(input: String, _ parse: (String) -> A, _ p1: (A) -> Void, _ p2: (A) -> Void) -> Void {
    var parsedInput: A?
    print("Input file parsed in \(time({ parsedInput = parse(input) }))s")
    print("Part 1 completed in \(time({ p1(parsedInput!) }))")
    print("Part 2 completed in \(time({ p2(parsedInput!) }))")
}

public func run<A>(input: String, _ parser: AnyParser<Substring, A>, _ p1: (A) -> Void, _ p2: (A) -> Void) {
    var parsedInput: A?
    let parseTime = time {
        let inView = input[...]
        let (p, rest) = parser.parse(inView)
        let leftovers = rest.trimmingCharacters(in: .whitespacesAndNewlines)
        guard leftovers.count == 0 else { fatalError("Parser has leftovers: \(leftovers)") }
        parsedInput = p
    }
    
    guard let parsedInput = parsedInput else { fatalError("Parser failed to parse anything!") }
    
    print("Input file parsed in \(parseTime)s")
    print("Part 1 completed in \(time({ p1(parsedInput) }))")
    print("Part 2 completed in \(time({ p2(parsedInput) }))")
}

public func ingestFile(_ filename: String) -> [String.SubSequence] {
    do {
        return try String(contentsOfFile: filename)
            .split(separator: "\n")
    } catch {
        preconditionFailure("Couldn't load file '\(filename)': \(error)")
    }
}

public func force<A>(_ a: A?) -> A { a! }

public let intify: (String.SubSequence) -> Int = pipe(String.init, Int.init, force)
public let divideBy: (Double) -> (Double) -> Double = flip(curry(/))
public let subtractBy: (Int) -> (Int) -> Int = flip(curry(-))

public func memoize<A, B>(_ f: @escaping (A) -> B) -> (A) -> B where A: Hashable {
    var memoization: [A: B] = [:]
    return { a in
        if let v = memoization[a] { return v }
        let v = f(a)
        memoization[a] = v
        return v
    }
}

/// Okay, what you actually want is a structure, generic along A, B, C..., which wraps a function
/// (A, B) -> C
/// and has access to a key generator
/// (A, B) -> Key
/// that it uses to generate lookups into the memoization dictionary (which is [Key: C])

public struct Memoize2<A, B, C, Key> where Key: Hashable {
    let wrappedFn: (A, B) -> C
    let keyGenerator: (A, B) -> Key
    var memoization: [Key: C] = [:]
    mutating func run(_ a: A, _ b: B) -> C {
        let key = keyGenerator(a, b)
        if let v = memoization[key] { return v }
        let v = wrappedFn(a, b)
        memoization[key] = v
        return v
    }
}

public struct Memoize3<A, B, C, D, Key> where Key: Hashable {
    public let wrappedFn: (A, B, C) -> D
    public let keyGenerator: (A, B, C) -> Key
    var memoization: [Key: D] = [:]
    
    public init (_ wrappedFn: @escaping (A, B, C) -> D, keyGenerator: @escaping (A, B, C) -> Key) {
        self.wrappedFn = wrappedFn
        self.keyGenerator = keyGenerator
    }
    
    public mutating func run(_ a: A, _ b: B, _ c: C) -> D {
        let key = keyGenerator(a, b, c)
        if let v = memoization[key] { return v }
        let v = wrappedFn(a, b, c)
        memoization[key] = v
        return v
    }
}

public struct Point {
    public var x: Int
    public var y: Int
    
    public init(_ x: Int, _ y: Int) {
        self.x = x
        self.y = y
    }
    
    public static func -(_ lhs: Point, _ rhs: Point) -> Point { .init(lhs.x - rhs.x, lhs.y - rhs.y) }
    public static func +(_ lhs: Point, _ rhs: Point) -> Point { .init(lhs.x + rhs.x, lhs.y + rhs.y) }
    public static func *(_ lhs: Point, _ rhs: Int) -> Point { .init(lhs.x * rhs, lhs.y * rhs) }
}

extension Point: Hashable { }

extension Point: Equatable {
    public static func ==(lhs: Point, rhs: Point) -> Bool {
        lhs.x == rhs.x && lhs.y == rhs.y
    }
}

extension Point: CustomStringConvertible {
    public var description: String { return "(\(x), \(y))" }
}

public extension String {
    subscript(index: Int) -> Character {
        self[String.Index(utf16Offset: index, in: self)]
    }
}
