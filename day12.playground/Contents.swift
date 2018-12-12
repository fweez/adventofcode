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
            _ = curr.add(result == "#")
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
        while self.pots & 7 != 0 {
            if self.pots & (7 << 62) != 0 {
                print(self.description)
                assert(false, "Must not shift up if top bits are taken")
            }
            
            self.pots = self.pots << 1
            self.idxOffset += 1
        }
        while self.pots & (7 << 62) != 0 {
            if self.pots & 7 != 0 {
                print(self.description)
                assert(false, "Must not shift down if bottom bits are taken")
            }
                
            self.pots = self.pots >> 1
            self.idxOffset -= 1
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
    for _ in 0..<20 {
        garden.incrementGeneration()
    }
    print("Sum of garden after 20 generations: \(garden.sum)")
    
    for i in 20..<50000000000 {
        if i % 100000 == 0 { print(".") }
        garden.incrementGeneration()
    }
    print("Sum of garden after lololol generations: \(garden.sum)")
} else {
    let testgarden = Garden(inputFile: "test")

    for _ in 0..<20 {
        testgarden.incrementGeneration()
        print(testgarden.description)
    }

    print("Sum of test garden: \(testgarden.sum)")

    let garden = Garden()
    for _ in 0..<20 {
        garden.incrementGeneration()
        print(testgarden.description)
    }
    print("Sum of garden after 20 generations: \(garden.sum)")
}
