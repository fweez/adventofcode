import Foundation

class PlantNode {
    var plant: PlantNode?
    var empty: PlantNode?
    var rule: String?
    
    func add(_ hasPlant: Bool) -> PlantNode {
        if hasPlant {
            if self.plant == nil { self.plant = PlantNode() }
            return self.plant!
        } else {
            if self.empty == nil { self.empty = PlantNode() }
            return self.empty!
        }
    }
    
    func get(_ hasPlant: Bool) -> PlantNode? {
        if hasPlant { return self.plant }
        else { return self.empty }
    }
    
    init(definitions: [Substring] = []) {
        for input in definitions {
            let inputs = input.split(separator: " ")
            let pattern = inputs[0]
            let result = inputs[2].trimmingCharacters(in: .whitespaces)
            var curr = self
            for c in pattern {
                curr = curr.add(c == "#")
            }
            let _ = curr.add(result == "#")
            curr.rule = String(input)
        }
    }
}

class Garden {
    var pots: Int64 = 0
    var rules: PlantNode!
    var idxOffset = 0
    
    init(inputFile: String = "input") {
        let input: String
        if let filePath = Bundle.main.path(forResource: inputFile, ofType: "txt"),
            let inp = try? String(contentsOfFile: filePath) {
            input = inp
        } else if let inp = try? String(contentsOfFile: inputFile) {
            input = inp
        } else {
            print("Couldn't load file")
            exit(1)
        }
        var lines = input.split(separator: "\n")
        let initialStateDefinition = lines.removeFirst().split(separator: ":")[1].trimmingCharacters(in: .whitespaces)
        for (idx, c) in initialStateDefinition.enumerated() {
            if c == "#" {
                self.pots |= 1 << idx
            }
        }
        self.padPots()
        self.rules = PlantNode(definitions: lines)
    }
    
    func padPots() {
        let firstplantidx = self.pots.firstIndex(of: true)!
        if firstplantidx < 5 {
            for _ in 0..<5-firstplantidx {
                self.pots.insert(false, at: 0)
            }
            self.idxOffset += 5-firstplantidx
        }
        if firstplantidx > 5 {
            self.pots.replaceSubrange(0..<firstplantidx, with: [false, false, false, false, false])
            self.idxOffset += 5-firstplantidx
        }
        //print("Idx offset now \(self.idxOffset)")
        let lastplantidx = self.pots.lastIndex(of: true)!
        if lastplantidx > self.pots.count - 6 {
            self.pots.append(contentsOf: Array(repeating: false, count: 5))
        }
    }
    
    func incrementGeneration() {
        var next: Int64 = 0
        for idx in 0..<60 {
            var curr = self.rules
            for b in idx..<idx+5 {
                curr = curr?.get(self.pots & 1<<b != 0)
                if curr == nil { break }
            }
            if curr != nil {
                //print("   \t" + Array(repeating: " ", count: idx).joined() + result.rule!)
                next |= 1 << idx+2
            }
        }
        self.pots = next
        self.padPots()
    }
    
    var sum: Int {
        var sum = 0
        for idx in 0..<64 {
            if self.pots & Int64(1)<<idx != 0 {
                sum += idx - self.idxOffset
            }
        }
        return sum
    }
    
    var description: String {
        var s = ""
        for idx in 0..<64 {
            if self.pots & Int64(1)<<idx != 0 {
                s.append("#")
            } else {
                s.append(".")
            }
        }
        return "\(s)\tsum:\(self.sum)"
    }
}

if CommandLine.arguments.count > 0 && CommandLine.arguments.last?.suffix(3) == "txt" {
    let arg = CommandLine.arguments.last!
    let garden = Garden(inputFile: arg)
    var sum = 0
    var generation = 0
    for _ in 0..<20 {
        garden.incrementGeneration()
        generation += 1
        sum = garden.sum
        print("[\(generation)]\t\(garden.description)")
    }
    print("Sum of garden after 20 generations: \(garden.sum)")

    
    sum = 0
    var diff = 0
    while true {
        diff = garden.sum - sum
        sum = garden.sum
        garden.incrementGeneration()
        generation += 1
        print("[\(generation)]\t\(garden.description)")
        let remaining = 50000000000 - generation
        print("Diff \(diff), projection for \(remaining) more generations is \(garden.sum + (remaining*diff))")
    }
    print("Sum of garden after \(generation) generations: \(garden.sum)")
} else {
    let testgarden = Garden(inputFile: "test")
    
    for generation in 0..<21 {
        print("[\(generation)]\t\(testgarden.description)")
        testgarden.incrementGeneration()
    }
}
