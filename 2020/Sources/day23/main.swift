import AOCShared
import Foundation
import Overture

class Cup {
    let contents: Int
    var next: Cup?
    var lower: Cup?
    
    init(_ i: Int) {
        contents = i
    }
    
    func reduce<A>(_ initial: A, _ f: (A, Int) -> A) -> A {
        var curr = self
        var accum = f(initial, contents)
        while let c = curr.next, c.contents != self.contents {
            accum = f(accum, c.contents)
            curr = c
        }
        return accum
    }
}

extension Cup: CustomStringConvertible {
    var description: String {
        reduce("") { s, i in
            s + "\(i) -> "
        }
    }
}

typealias ParsedStructure = Cup

func parse(_ input: String) -> ParsedStructure {
    let cups = input
        .compactMap(pipe(String.init, Int.init))
        .map(Cup.init)
    cups
        .enumerated()
        .prefix(cups.count - 1)
        .forEach {
            $0.element.next = cups[$0.offset + 1]
        }
    cups.last!.next = cups.first!
    cups
        .forEach { cup in
            cup.lower = findDest(cup)
        }
    return cups.first!
}

func min(_ head: Cup) -> Int { head.reduce(Int.max, min) }
func max(_ head: Cup) -> Int { head.reduce(Int.min, max) }

func take3(_ head: Cup) -> Cup {
    let r = head.next!
    head.next = r.next!.next!.next!
    r.next!.next!.next = r
    return r
}

func findLargestBelow(target: Int, in head: Cup) -> Cup? {
    var curr = head.next!
    var dest: Cup?
    while curr.contents != head.contents {
        if curr.contents < target && curr.contents > (dest?.contents ?? Int.min) { dest = curr }
        curr = curr.next!
    }
    return dest
}

func findDest(_ head: Cup) -> Cup {
    findLargestBelow(target: head.contents, in: head) ?? findLargestBelow(target: Int.max, in: head)!
}

func tail(_ head: Cup) -> Cup {
    var curr = head
    while curr.next!.contents != head.contents {
        curr = curr.next!
    }
    return curr
}

func insert(_ inserted: Cup, nextTo head: Cup) {
    let n = head.next!
    let t = tail(inserted)
    head.next = inserted
    t.next = n
}

func find(_ target: Int, in head: Cup) -> Bool {
    if head.contents == target { return true }
    var curr = head.next!
    while curr.contents != head.contents {
        if curr.contents == target { return true }
        curr = curr.next!
    }
    return false
}

func move(_ head: Cup) -> Cup {
    let removed = take3(head)
    var dest = head.lower!
    while find(dest.contents, in: removed) {
        dest = dest.lower!
    }
    insert(removed, nextTo: dest)
    return head.next!
}

func order(_ head: Cup) -> String {
    findLargestBelow(target: min(head) + 1, in: head)!
        .next!
        .reduce("")  { s, i in s + "\(i)" }
        .filter { $0 != "1" } // this is stupid
}

func part1(_ head: ParsedStructure) {
    var curr = head
    (0..<100)
        .forEach { _ in curr = move(curr) }
    let result = order(curr)
    assert(Int(result)! < 576413892, "bad guess")
    print("Part 1: \(result)")
}

func p2(_ head: ParsedStructure) -> Int {
    let one = findLargestBelow(target: 2, in: head)!
    var curr = tail(head)
    var lower = findLargestBelow(target: 10, in: head)!
    (10...1000000)
        .forEach {
            let c = Cup($0)
            curr.next = c
            c.lower = lower
            curr = c
            lower = c
        }
    curr.next = head
    one.lower = curr
    (1...10000000)
        .forEach { turn in
            if turn % 100000 == 0 { print("Turn \(turn)") }
            curr = move(curr)
        }
    let v1 = one.next!.contents
    let v2 = one.next!.next!.contents
    let result = v1 * v2
    return result
}

func part2(_ head: ParsedStructure) {
    let result = p2(head)
    assert(result > 84272730752, "bad guess")
    print("Part 2: \(result)") }

func test1() {
    let head = parse("389125467")
    var curr = head
    (0..<10)
        .forEach { _ in curr = move(curr) }
    let result1 = order(head)
    assert(result1 == "92658374")
    (10..<100)
        .forEach { _ in curr = move(curr) }
    let result2 = order(head)
    assert(result2 == "67384529")
    
}

func test2() {
    let result = p2(parse("389125467"))
    assert(result == 149245887792)
}

test1()
test2()
run(input: "562893147", parse, part1, part2)
