import Foundation
import PlaygroundSupport

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
            curr.add(result == "#")
            curr.rule = String(input)
        }
    }
}

class Garden {
    var pots: [Bool]!
    var rules: PlantNode!
    var idxOffset = 0
    
    init(inputFile: String = "input") {
        guard let filePath = Bundle.main.path(forResource: inputFile, ofType: "txt"),
            let input = try? String(contentsOfFile: filePath) else {
                print("Couldn't load file")
                exit(1)
        }
        
        var lines = input.split(separator: "\n")
        let initialStateDefinition = lines.removeFirst().split(separator: ":")[1].trimmingCharacters(in: .whitespaces)
        self.pots = initialStateDefinition.map({ $0 == "#" })
        self.padPots()
        print(lines.joined(separator: "\n"))
        self.rules = PlantNode(definitions: lines)
    }
    
    func padPots() {
        if !self.pots.prefix(3).allSatisfy({ !$0 }) {
            self.pots.insert(false, at: 0)
            self.pots.insert(false, at: 0)
            self.pots.insert(false, at: 0)
            self.idxOffset += 3
        }
        if !self.pots.suffix(3).allSatisfy({ !$0 }) {
            self.pots.append(false)
            self.pots.append(false)
            self.pots.append(false)
        }
    }
    
    func incrementGeneration() {
        var next: [Bool] = Array(self.pots)
        for idx in 0..<pots.count-4 {
            var curr = self.rules
            for pot in self.pots[idx..<idx+5] {
                curr = curr?.get(pot)
                if curr == nil { break }
            }
            if let result = curr {
                //print("   \t" + Array(repeating: " ", count: idx).joined() + result.rule!)
                next[idx+2] = result.empty == nil
            } else {
                next[idx+2] = false
            }
        }
        self.pots = next
        self.padPots()
    }
    
    var sum: Int {
        var sum = 0
        for idx in 0..<self.pots.count {
            if self.pots[idx] {
                sum += idx - self.idxOffset
            }
        }
        return sum
    }
    
    var description: String {
        return self.pots.map({ $0 ? "#" : "."}).joined() + "\tsum:\(self.sum)"
    }
}

let testgarden = Garden(inputFile: "test")

for generation in 0..<21 {
    print("[\(generation)]\t\(testgarden.description)")
    testgarden.incrementGeneration()
}

let garden = Garden()
for generation in 0..<21 {
    print("[\(generation)]\t\(garden.description)")
    garden.incrementGeneration()
}
