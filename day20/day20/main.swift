//
//  main.swift
//  day20
//
//  Created by Ryan Forsythe on 12/20/18.
//  Copyright © 2018 Zero Gravitas. All rights reserved.
//

import Foundation

print("Hello, World!")

enum ParseError: Error {
    case BadCarat(String)
    case BadDollar(String)
    case Unconsumed(String)
    case BadInput(String)
}

class PathSegment {
    var direction: Character? = nil
    var next: [PathSegment] = []
    
    static func consume(input: inout String) throws -> [(head: PathSegment, tail: PathSegment)] {
        print("Consuming \(input)")
        let initialHead: PathSegment = PathSegment()
        let initialTail: PathSegment = initialHead
        var stack = [(initialHead, initialTail)]
        
        while input.count > 0 {
            let character = input.first!
            switch character {
            case "^":
                print("Carat")
                input.removeFirst()
            case "N", "E", "W", "S":
                let new = PathSegment()
                new.direction = input.removeFirst()
                let (head, tail) = stack.removeLast()
                tail.next.append(new)
                stack.append((head, new))
                print("New node \(new.direction!), now \(input)")
            case "(":
                print("Starting subpaths")
                input.removeFirst()
                let subtrees = try PathSegment.consume(input: &input)
                let subtreeTail = PathSegment()
                for t in subtrees.map({ $0.1 }) {
                    t.next = [subtreeTail]
                }
                let (head, tail) = stack.removeLast()
                tail.next.append(contentsOf: subtrees.map({ $0.0 }))
                stack.append((head, subtreeTail))
                print("New node of \(subtrees.count) subtrees, now \(input)")
            case "|":
                print("Pipe")
                input.removeFirst()
                let subtreeHead: PathSegment = PathSegment()
                stack.append((subtreeHead, subtreeHead))
            case ")":
                input.removeFirst()
                print("End subtree with )")
                return stack
            case "$":
                input.removeFirst()
                print("End tree with $")
                return stack
            default:
                throw ParseError.BadInput("\(input): character \(character)")
            }
        }
        throw ParseError.BadInput("\(input))")
        return stack
    }
    
    var longestPath: Int {
        var shortestSubpath = Int.max
        for subpath in self.next {
            let subpathLen = subpath.longestPath
            //print("subpath \(subpath.direction ?? "-") has length \(subpathLen)")
            shortestSubpath = min(subpathLen, shortestSubpath)
        }
        
        if shortestSubpath == Int.max { shortestSubpath = 0 }
        
        if self.direction != nil {
            return 1 + shortestSubpath
        } else {
            return shortestSubpath
        }
    }
}

extension PathSegment: CustomStringConvertible {
    var description: String {
        let nextDesc: String
        if self.next.count > 1 {
            nextDesc = "(\(self.next.map( { $0.description }).joined(separator: "|")))"
        } else if self.next.count == 1 {
            nextDesc = self.next.first!.description
        } else {
            nextDesc = "$"
        }
        return String(self.direction ?? "*") + nextDesc
    }
}

class RoomMap {
    var head: PathSegment!
    
    convenience init?(inputFile: String) throws {
        self.init()
        if let filePath = Bundle.main.path(forResource: inputFile, ofType: "txt"),
            var input = try? String(contentsOfFile: filePath) {
            _ = try self.load(&input)
        } else if var input = try? String(contentsOfFile: inputFile) {
            _ = try self.load(&input)
        } else {
            print("Couldn't load file")
            return nil
        }
    }
    
    func load(_ input: inout String) throws {
        let stack = try PathSegment.consume(input: &input)
        if stack.count != 1 {
            throw ParseError.Unconsumed(input)
        }
        self.head = stack.first!.0
    }
}

let mapper = RoomMap()
var input = "^WNE$"
try mapper.load(&input)
assert(mapper.head.longestPath == 3)
input = "^W(NE)S$"
try mapper.load(&input)
assert(mapper.head.longestPath == 4)
input = "^W(N|EEE)S$"
do {
    try mapper.load(&input)
} catch {
    print(error)
}
assert(mapper.head.longestPath == 3)

let tests = [("^ENWWW(NEEE|SSE(EE|N))$", 10),
    ("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$", 18),
    ("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$", 23),
    ("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$", 31),
    ]
for (regex, expectedPathLen) in tests {
    input = regex
    print(input)
    try mapper.load(&input)
    print(mapper.head!)
    print(mapper.head.longestPath)
}
