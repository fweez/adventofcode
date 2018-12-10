import Foundation

class Node {
    var value: Int = -1
    weak var next: Node? = nil
    weak var prev: Node? = nil
    
    init(_ value: Int) {
        self.value = value
    }
    
    func append(_ value: Int) -> Node {
        let new = Node(value)
        let oldnext = self.next!
        oldnext.prev = new
        self.next = new
        new.next = oldnext
        new.prev = self
        return new
    }
    
    func remove() -> Node {
        self.prev!.next = self.next
        self.next!.prev = self.prev
        return self.next!
    }
    
    var description: String {
        var curr = self
        var accum = ""
        while true {
            accum.append("\(curr.prev!.value)-\(curr.value)-\(curr.next!.value) ")
            curr = curr.next!
            if curr.value == self.value { break }
        }
        return accum
    }
}

func playgame(playercount: Int, lastmarble: Int) -> Int {
    var nodes: [Node] = [Node(0)]
    var curr = nodes[0]
    curr.next = curr
    curr.prev = curr
    var players: [Int] = Array(repeating: 0, count: playercount)
    var currentplayer = 0
    var marble = 0
    
    while marble < lastmarble {
        marble += 1
        if marble % 23 == 0 {
            for _ in 0..<7 {
                curr = curr.prev!
            }
            players[currentplayer] += marble + curr.value
            curr = curr.remove()
        } else {
            curr = curr.next!
            let newhead = curr.append(marble)
            nodes.append(newhead)
            curr = newhead
        }
        currentplayer = (currentplayer + 1) % players.count
    }
    
    players.sort()
    return players.last!
}

let tests = [(9,25, 32), (10, 1618, 8317), (13, 7999, 146373), (17, 1104, 2764), (21, 6111, 54718), (30, 5807, 37305)];

for (playercount, lastmarble, answer) in tests {
    let score = playgame(playercount: playercount, lastmarble: lastmarble)
    print("TEST: \(playercount) players, \(lastmarble) last marble: high score \(score)")
    assert(score == answer)
    
}

let scoreA = playgame(playercount: 411, lastmarble: 71058)
print("Part A high score: \(scoreA)")
let scoreB = playgame(playercount: 411, lastmarble: 71058 * 100)
print("Part B high score: \(scoreB)")
