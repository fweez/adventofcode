import Foundation

public protocol ParserProtocol {
    associatedtype Output
    associatedtype Seq: Collection
    
    var run: (inout Seq.SubSequence) -> Output? { get }
}

public struct Parser<A, B>: ParserProtocol where B: Collection {
    public typealias Output = A
    public typealias Seq = B
    
    public let run: (inout B.SubSequence) -> A?
}

public extension Parser {
    func map<B>(_ f: @escaping (A) -> B) -> Parser<B, Seq> {
        Parser<B, Seq> { str -> B? in
            self.run(&str)
                .map(f)
        }
    }
    
    func flatMap<B>(_ f: @escaping (A) -> Parser<B, Seq>) -> Parser<B, Seq> {
        Parser<B, Seq> { input -> B? in
            let original = input
            let matchA = self.run(&input)
            guard let matchB = matchA.map(f)?.run(&input) else {
                input = original
                return nil
            }
            return matchB
        }
    }
    
    func runStatic(_ s: Seq) -> Output? {
        var ms = s[...]
        return self.run(&ms)
    }
}

public extension Parser {
    func debug() -> Parser<A, B> {
        Parser<A, B> { i in
            guard let v = self.run(&i) else {
                print("failed at input \(i)")
                return nil
            }
            return v
        }
    }
}

public func zip<A, B, Seq>(_ a: Parser<A, Seq>, _ b: Parser<B, Seq>) -> Parser<(A, B), Seq> {
    Parser<(A, B), Seq> { str -> (A, B)? in
        let orig = str
        guard let matchA = a.run(&str) else { return nil }
        guard let matchB = b.run(&str) else {
            str = orig
            return nil
        }
        return (matchA, matchB)
    }
}

public func zip<A, B, C, Seq>(
    _ a: Parser<A, Seq>,
    _ b: Parser<B, Seq>,
    _ c: Parser<C, Seq>)
    -> Parser<(A, B, C), Seq> {
    zip(a, zip(b, c)).map { a, bc in (a, bc.0, bc.1) }
}

public func zip<A, B, C, D, Seq>(
    _ a: Parser<A, Seq>,
    _ b: Parser<B, Seq>,
    _ c: Parser<C, Seq>,
    _ d: Parser<D, Seq>)
    -> Parser<(A, B, C, D), Seq> {
    zip(a, zip(b, c, d)).map { a, bcd in (a, bcd.0, bcd.1, bcd.2) }
}

public func zip<A, B, C, D, E, Seq>(
    _ a: Parser<A, Seq>,
    _ b: Parser<B, Seq>,
    _ c: Parser<C, Seq>,
    _ d: Parser<D, Seq>,
    _ e: Parser<E, Seq>) -> Parser<(A, B, C, D, E), Seq> {
    zip(a, zip(b, c, d, e)).map { a, bcde in (a, bcde.0, bcde.1, bcde.2, bcde.3) }
}

public func zip<A, B, C, D, E, F, Seq>(
    _ a: Parser<A, Seq>,
    _ b: Parser<B, Seq>,
    _ c: Parser<C, Seq>,
    _ d: Parser<D, Seq>,
    _ e: Parser<E, Seq>,
    _ f: Parser<F, Seq>) -> Parser<(A, B, C, D, E, F), Seq> {
    zip(a, zip(b, c, d, e, f)).map { a, bcdef in (a, bcdef.0, bcdef.1, bcdef.2, bcdef.3, bcdef.4) }
}

public func zip<A, B, C, D, E, F, G, Seq>(
    _ a: Parser<A, Seq>,
    _ b: Parser<B, Seq>,
    _ c: Parser<C, Seq>,
    _ d: Parser<D, Seq>,
    _ e: Parser<E, Seq>,
    _ f: Parser<F, Seq>,
    _ g: Parser<G, Seq>
) -> Parser<(A, B, C, D, E, F, G), Seq> {
    zip(a, zip(b, c, d, e, f, g)).map { a, bcdefg in (a, bcdefg.0, bcdefg.1, bcdefg.2, bcdefg.3, bcdefg.4, bcdefg.5) }
}

@resultBuilder
public struct InstructionBuilder {
    static func buildBlock<A, B, Seq: Collection>(
        _ a: Parser<A, Seq>,
        _ b: Parser<B, Seq>)
    -> Parser<(A, B), Seq> { zip(a, b) }
    
    static func buildBlock<A, B, C, Seq>(
        _ a: Parser<A, Seq>,
        _ b: Parser<B, Seq>,
        _ c: Parser<C, Seq>)
    -> Parser<(A, B, C), Seq> { zip(a, b, c) }
    
    static func buildBlock<A, B, C, D, Seq>(
        _ a: Parser<A, Seq>,
        _ b: Parser<B, Seq>,
        _ c: Parser<C, Seq>,
        _ d: Parser<D, Seq>)
        -> Parser<(A, B, C, D), Seq> { zip(a, b, c, d) }
    
    static func buildBlock<A, B, C, D, E, Seq>(
        _ a: Parser<A, Seq>,
        _ b: Parser<B, Seq>,
        _ c: Parser<C, Seq>,
        _ d: Parser<D, Seq>,
        _ e: Parser<E, Seq>)
    -> Parser<(A, B, C, D, E), Seq> { zip(a, b, c, d, e) }
    
    static func buildBlock<A, B, C, D, E, F, Seq>(
        _ a: Parser<A, Seq>,
        _ b: Parser<B, Seq>,
        _ c: Parser<C, Seq>,
        _ d: Parser<D, Seq>,
        _ e: Parser<E, Seq>,
        _ f: Parser<F, Seq>)
    -> Parser<(A, B, C, D, E, F), Seq> { zip(a, b, c, d, e, f) }
    
    static func buildBlock<A, B, C, D, E, F, G, Seq>(
        _ a: Parser<A, Seq>,
        _ b: Parser<B, Seq>,
        _ c: Parser<C, Seq>,
        _ d: Parser<D, Seq>,
        _ e: Parser<E, Seq>,
        _ f: Parser<F, Seq>,
        _ g: Parser<G, Seq>)
    -> Parser<(A, B, C, D, E, F, G), Seq> { zip(a, b, c, d, e, f, g) }
}

func zip<A, B>(@InstructionBuilder f: () -> Parser<A, B>) -> Parser<A, B> { f() }

public func zip<A, B, C, Seq>(
    _ a: Parser<A, Seq>,
    _ b: Parser<B, Seq>,
    with f: @escaping (A, B) -> C)
    -> Parser<C, Seq> {
    zip(a, b).map(f)
}

public func zip<A, B, C, D, Seq>(
    _ a: Parser<A, Seq>,
    _ b: Parser<B, Seq>,
    _ c: Parser<C, Seq>,
    with f: @escaping (A, B, C) -> D)
    -> Parser<D, Seq> {
    zip(a, b, c).map(f)
}

public func zip<A, B, C, D, E, Seq>(
    _ a: Parser<A, Seq>,
    _ b: Parser<B, Seq>,
    _ c: Parser<C, Seq>,
    _ d: Parser<D, Seq>,
    with f: @escaping (A, B, C, D) -> E)
    -> Parser<E, Seq> {
    zip(a, b, c, d).map(f)
}

public func zip<A, B, C, D, E, F, Seq>(
    _ a: Parser<A, Seq>,
    _ b: Parser<B, Seq>,
    _ c: Parser<C, Seq>,
    _ d: Parser<D, Seq>,
    _ e: Parser<E, Seq>,
    with f: @escaping (A, B, C, D, E) -> F)
    -> Parser<F, Seq> {
    zip(a, b, c, d, e).map(f)
}

public func zip<A, B, C, D, E, F, G, Seq>(
    _ a: Parser<A, Seq>,
    _ b: Parser<B, Seq>,
    _ c: Parser<C, Seq>,
    _ d: Parser<D, Seq>,
    _ e: Parser<E, Seq>,
    _ f: Parser<F, Seq>,
    with g: @escaping (A, B, C, D, E, F) -> G)
    -> Parser<G, Seq> {
    zip(a, b, c, d, e, f).map(g)
}

public func zip<A, B, C, D, E, F, G, H, Seq>(
    _ a: Parser<A, Seq>,
    _ b: Parser<B, Seq>,
    _ c: Parser<C, Seq>,
    _ d: Parser<D, Seq>,
    _ e: Parser<E, Seq>,
    _ f: Parser<F, Seq>,
    _ g: Parser<G, Seq>,
    with h: @escaping (A, B, C, D, E, F, G) -> H)
    -> Parser<H, Seq> {
        zip(a, b, c, d, e, f, g).map(h)
}

extension Parser {
    static func always(_ a: A) -> Self {
        return .init() { _ in a }
    }
    
    static var never: Parser {
        return Parser { _ in nil }
    }
}

public func zeroOrMore<A, Seq>(_ p: Parser<A, Seq>, separatedBy s: Parser<Void, Seq>) -> Parser<[A], Seq> {
    return Parser<[A], Seq> { input in
        var original = input
        var matches: [A] = []
        while let match = p.run(&input) {
            original = input
            matches.append(match)
            if s.run(&input) == nil { return matches }
        }
        input = original
        return matches
    }
}

public func zeroOrMore<A, Seq>(_ p: Parser<A, Seq>) -> Parser<[A], Seq> {
    return Parser<[A], Seq> { input in
        var matches: [A] = []
        while let match = p.run(&input) {
            matches.append(match)
        }
        return matches
    }
}

public func oneOf<A, Seq>(_ parsers: [Parser<A, Seq>]) -> Parser<A, Seq> {
    return Parser<A, Seq> { str -> A? in
        for p in parsers {
            if let match = p.run(&str) { return match }
        }
        return nil
    }
}

public func optionalPrefix<A>(while p: @escaping (A.Element) -> Bool) -> Parser<A.SubSequence, A> where A: Collection {
    Parser<A.SubSequence, A> { input in
        let prefix = input.prefix(while: p)
        guard prefix.count > 0 else { return nil }
        input.removeFirst(prefix.count)
        return prefix
    }
}

public func hasPrefix<A>(while p: @escaping (A.Element) -> Bool) -> Parser<A.SubSequence, A> where A: Collection {
    optionalPrefix(while: p)
        .flatMap { str in
            guard str.count > 0 else { return .never }
            return .always(str)
    }
}

public extension Collection where Element: Equatable {
    func hasPrefix<A>(_ seq: A) -> Bool where A: Collection, A.Element == Self.Element {
        guard self.count >= seq.count else {
            return false }
        return zip(self.prefix(seq.count), seq)
            .allSatisfy(==)
    }
}

public func literal<A>(_ literalSequence: A) -> Parser<Void, A> where A: Collection, A.Element: Equatable {
    return Parser<Void, A> { input in
        guard input.hasPrefix(literalSequence) else {
            return nil
        }
        input.removeFirst(literalSequence.count)
        return ()
    }
}

public func literal<A>(_ literal: A.Element) -> Parser<Void, A> where A: Collection, A.Element: Equatable {
    return Parser<Void, A> { input in
        guard input.first == literal else {
            return nil
        }
        input.removeFirst()
        return ()
    }
}

public func literal<A, B>(_ literalSequence: A, _ produces: B) -> Parser<B, A> where A: Collection, A.Element: Equatable {
    literal(literalSequence).flatMap { _ in .always(produces) }
}

public func literal<A, B>(_ l: A.Element, _ produces: B) -> Parser<B, A> where A: Collection, A.Element: Equatable {
    literal(l).flatMap { _ in .always(produces) }
}

public func literal<A>(predicate: @escaping (A.Element) -> Bool) -> Parser<Void, A> {
    return Parser<Void, A> { input in
        guard let f = input.first, predicate(f) else { return nil }
        input.removeFirst()
        return ()
    }
}

public func take<A, B>(_ n: Int, _ p: Parser<A, B>) -> Parser<[A], B> {
    Parser<[A], B> { str -> [A]? in
        let matches = (0..<n)
            .compactMap { _ in p.run(&str) }
        guard matches.count == n else { return nil }
        return matches
    }
}

public let intParser = zip(
    zeroOrMore(literal("-")),
    optionalPrefix(while: { $0.isNumber }))
    .map { sgn, val -> Int in
        if sgn.count > 0 { return Int(val)! * -1 }
        else { return Int(val)! }
}

public let alphaParser: Parser<Substring, String> = hasPrefix(while: { $0.isLetter })

public let nonWhitespaceParser: Parser<Substring, String> = hasPrefix(while: { $0.isWhitespace == false })
 
public let characterParser = Parser<Character, String> { input in
    guard let c = input.first else { return nil }
    input.removeFirst()
    return c
}

