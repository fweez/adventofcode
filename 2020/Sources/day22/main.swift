import AOCShared
import Foundation
import Overture

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

typealias ParsedStructure = ([Int], [Int])

func parse(_ input: String) -> ParsedStructure {
    let deck = zip(
        oneOf([literal("Player 1:\n"), literal("Player 2:\n")]),
        zeroOrMore(intParser, separatedBy: literal("\n"))
    ).map { _, i in i }
    return zip(deck, literal("\n\n"), deck).map { a, _, b in (a, b) }
        .runStatic(input) ?? ([], [])
    
}

func gameP1(_ decks: ([Int], [Int])) -> ([Int], [Int]) {
    guard let a = decks.0.first, let b = decks.1.first else {
        return decks
    }
    if a > b {
        return gameP1((decks.0.suffix(from: 1) + [a, b], Array(decks.1.suffix(from: 1))))
    } else {
        return gameP1((Array(decks.0.suffix(from: 1)), decks.1.suffix(from: 1) + [b, a]))
    }
}

func score(_ stack: [Int]) -> Int {
    stack
        .reversed()
        .enumerated()
        .reduce(0) { a, t in a + ((t.offset + 1) * t.element) }
}

func part1(_ parsedInput: ParsedStructure) {
    let (a, b) = gameP1(parsedInput)
    let count: Int
    if a.count > 0 {
        count = score(a)
    } else {
        count = score(b)
    }
    assert(count < 34924, "guess 1")
    print("Part 1: \(count)")
}

struct DeckKey: Hashable {
    let decks: ParsedStructure
    
    static func == (lhs: DeckKey, rhs: DeckKey) -> Bool {
        lhs.decks.0 == rhs.decks.0 &&
            lhs.decks.1 == rhs.decks.1
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(decks.0)
        hasher.combine(decks.1)
    }
}

struct GameState: Hashable {
    let deckKey: Int
    let seenKeys: Set<Int>
}

var memo: [Int: ParsedStructure] = [:]

func gameP2(_ decks: ParsedStructure) -> ParsedStructure {
    // memoization: if there was a previous game that had these decks, return the end value:
    let origK = DeckKey(decks: decks).hashValue
    if let m = memo[origK] { return m }
    
    var p1Deck = decks.0
    var p2Deck = decks.1
    var seen: Set<Int> = []
    
    while true {
        // If the previous pass emptied one of the decks, return immediately
        guard p1Deck.count > 0 && p2Deck.count > 0 else {
            let r = (p1Deck, p2Deck)
            memo[origK] = r
            return r
        }
    
        // if there was a previous round in this game that had exactly the same cards in the same order in the same players' decks, the game instantly ends in a win for player 1.
        let k = DeckKey(decks: (p1Deck, p2Deck)).hashValue
        if seen.contains(k) {
            let r = (p1Deck, [Int]())
            memo[origK] = r
            return r
        }
        seen.insert(k)
        
        // the players begin the round by each drawing the top card of their deck as normal.
        let a = p1Deck.removeFirst()
        let b = p2Deck.removeFirst()
        
        // If both players have at least as many cards remaining in their deck as the value of the card they just drew, the winner of the round is determined by playing a new game of Recursive Combat
        if a <= p1Deck.count && b <= p2Deck.count {
            let rP1Deck = Array(p1Deck.prefix(a))
            let rP2Deck = Array(p2Deck.prefix(b))
            let (rA, rB) = gameP2((rP1Deck, rP2Deck))
            if rA.count > rB.count {
                p1Deck.append(contentsOf: [a, b])
            } else {
                p2Deck.append(contentsOf: [b, a])
            }
        } else {
            //Otherwise, at least one player must not have enough cards left in their deck to recurse; the winner of the round is the player with the higher-value card.
            if a > b {
                p1Deck.append(contentsOf: [a, b])
            } else {
                p2Deck.append(contentsOf: [b, a])
            }
        }
    }
}

func part2(_ parsedInput: ParsedStructure) {
    let (a, b) = gameP2(parsedInput)
    let count: Int
    if a.count > 0 {
        count = score(a)
    } else {
        count = score(b)
    }
    assert(count > 30347, "guess 1")
    assert(count < 32629, "guess 2")
    print("Part 2: \(count)")
}

func test2_1() {
    let decks = parse("""
        Player 1:
        9
        2
        6
        3
        1
        
        Player 2:
        5
        8
        4
        7
        10
        """)
    let (a, b) = gameP2(decks)
    assert(score(a) == 0)
    assert(score(b) == 291)
    print("Test 2_1 complete")
}

func test2_2() {
    let decks = parse("""
            Player 1:
            43
            19
            
            Player 2:
            2
            29
            14
            """)
    let (a, b) = gameP2(decks)
    print("Test 2_2 complete")
}

test2_1()
test2_2()
run(input: input, parse, part1, part2)
