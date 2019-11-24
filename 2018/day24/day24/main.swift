//
//  main.swift
//  day24
//
//  Created by Ryan Forsythe on 12/26/18.
//  Copyright © 2018 Zero Gravitas. All rights reserved.
//

import Foundation

enum ParseError: Error {
    case BadInput(String)
}

class Group {
    var id: UUID
    var unitCount: Int
    var hitPoints: Int
    var baseAttackDamage: Int
    var attackType: String
    var immunities: [String] = []
    var weaknesses: [String] = []
    var initiative: Int
    var boost: Int = 0
    
    var attackDamage: Int { return self.baseAttackDamage + self.boost }
    
    init(input: String) throws {
        self.id = UUID()
        func intFromRegex(input: String, regex: String, wordIdx: Int = 0) throws -> Int {
            guard let r = input.range(of: regex, options: .regularExpression), let i = Int(String(input[r].split(separator: " ")[wordIdx])) else { throw ParseError.BadInput("On \(regex) in: \(input)") }
            return i
        }
        self.unitCount = try intFromRegex(input: input, regex: "\\d+ units")
        self.hitPoints = try intFromRegex(input: input, regex: "\\d+ hit points")
        self.baseAttackDamage = try intFromRegex(input: input, regex: "does \\d+", wordIdx: 1)
        self.initiative = try intFromRegex(input: input, regex: "initiative \\d+", wordIdx: 1)
        
        guard let r = input.range(of: "\\w+ damage", options: .regularExpression) else { throw ParseError.BadInput("On damage type in: \(input)") }
        self.attackType = String(input[r].split(separator: " ")[0])
        
        func typeList(kind: String) throws -> [String] {
            let regexString = "\(kind) to ([\\w, ]+)[;\\)]"
            let regex = try NSRegularExpression(pattern: regexString, options: [])
            let immunityMatches = regex.matches(in: input, options: [], range: NSMakeRange(0, input.count))
            guard let match = immunityMatches.first, match.numberOfRanges == 2 else { return [] }
            let r = match.range(at: 1)
            let s = String.UTF16Index(encodedOffset: r.location)
            let e = String.UTF16Index(encodedOffset: r.location + r.length)
            let list = input[s..<e]
            return list.split(separator: ",").map { String($0).trimmingCharacters(in: .whitespaces) }
        }
        
        self.immunities = try typeList(kind: "immune")
        self.weaknesses = try typeList(kind: "weak")
    }
    
    var effectivePower: Int {
        return self.unitCount * self.attackDamage
    }
    
    func damageDealt(to enemy: Group) -> Int {
        if enemy.weaknesses.contains(self.attackType) { return self.unitCount * self.attackDamage * 2 }
        if enemy.immunities.contains(self.attackType) { return 0 }
        return self.unitCount * self.attackDamage
    }
    
    func chooseTarget(in groups: Set<Group>) -> Group? {
        return groups.filter( { self.damageDealt(to: $0) > 0 }).sorted(by: { (a, b) -> Bool in
            let dmgA = self.damageDealt(to: a)
            let dmgB = self.damageDealt(to: b)
            if dmgA == dmgB {
                if a.effectivePower == b.effectivePower {
                    return a.initiative > b.initiative
                } else {
                    return a.effectivePower > b.effectivePower
                }
            } else {
                return dmgA > dmgB
            }
        }).first
    }
}

extension Group: Equatable {
    static func == (lhs: Group, rhs: Group) -> Bool {
        return lhs.id == rhs.id
    }
}

extension Group: Hashable {
    func hash(into hasher: inout Hasher) {
        hasher.combine(self.id)
    }
}

extension Group: CustomStringConvertible {
    var description: String {
        return "\(self.id): \(self.unitCount) units each with \(self.hitPoints) hit points (weak to \(self.weaknesses); immune to \(self.immunities)) with an attack that does \(self.attackDamage) \(self.attackType) damage at initiative \(self.initiative), boost \(self.boost)"
    }
}

struct Army {
    var groups: [Group] = []
    var boost: Int = 0 {
        didSet {
            for group in self.groups { group.boost = self.boost }
        }
    }
    
    init(input: String) throws {
        for line in input.split(separator: "\n") {
            self.groups.append(try Group(input: String(line)))
        }
    }
}

struct War {
    var immuneSystem: Army
    var infection: Army
    
    func getTargets() -> [Group: Group] {
        var attackMap: [Group: Group] = [:]
        
        func choose(attackers: Army, defenders: Army) {
            var candidates = Set(defenders.groups)
            
            func groupCmp(a: Group, b: Group) -> Bool {
                if a.effectivePower == b.effectivePower {
                    return a.initiative > b.initiative
                } else {
                    return a.effectivePower > b.effectivePower
                }
            }
            
            for attackGroup in attackers.groups.sorted(by: groupCmp) {
                if let candidate = attackGroup.chooseTarget(in: candidates) {
                    //print("\(attackGroup.id) chose \(candidate.id)")
                    attackMap[attackGroup] = candidate
                    candidates.remove(candidate)
                    continue
                }
            }
        }
        
        choose(attackers: self.immuneSystem, defenders: self.infection)
        choose(attackers: self.infection, defenders: self.immuneSystem)
        
        return attackMap
    }
    
    func attackPhase(_ attackMap: [Group: Group]) {
        for attacker in attackMap.keys.sorted(by: { $0.initiative > $1.initiative }) {
            guard attacker.effectivePower > 0 else { continue }
            let defender = attackMap[attacker]!
            let damage = attacker.damageDealt(to: defender)
            let unitsLost = min(damage / defender.hitPoints, defender.unitCount)
            //print("\(attacker.id) attacks \(defender.id) for \(damage) hitpoints, destroying \(unitsLost) units")
            defender.unitCount -= unitsLost
        }
    }
    
    mutating func fight() {
        let attackMap = self.getTargets()
        self.attackPhase(attackMap)
        self.immuneSystem.groups = self.immuneSystem.groups.filter( { $0.effectivePower > 0 })
        self.infection.groups = self.infection.groups.filter( { $0.effectivePower > 0 })
    }
}

let testing = false

if testing {
    let groupA = try Group(input: "1614 units each with 8016 hit points (immune to slashing, fire; weak to radiation, bludgeoning) with an attack that does 48 fire damage at initiative 9")
    assert(groupA.unitCount == 1614)
    assert(groupA.hitPoints == 8016)
    assert(groupA.baseAttackDamage == 48)
    assert(groupA.initiative == 9)
    assert(groupA.attackType == "fire")
    assert(groupA.immunities == ["slashing", "fire"])
    assert(groupA.weaknesses == ["radiation", "bludgeoning"])
    
    let groupB = try Group(input: "3730 units each with 5611 hit points (immune to bludgeoning) with an attack that does 14 radiation damage at initiative 16")
    assert(groupB.unitCount == 3730)
    assert(groupB.hitPoints == 5611)
    assert(groupB.baseAttackDamage == 14)
    assert(groupB.initiative == 16)
    assert(groupB.attackType == "radiation")
    assert(groupB.immunities == ["bludgeoning"])
    assert(groupB.weaknesses == [])
    
    let groupC = try Group(input: "3730 units each with 5611 hit points (weak to bludgeoning) with an attack that does 14 radiation damage at initiative 16")
    assert(groupC.immunities == [])
    assert(groupC.weaknesses == ["bludgeoning"])
    
    let testImmuneInput = """
    17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
    989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3
    """
    let testImmuneArmy = try Army(input:testImmuneInput)
    assert(testImmuneArmy.groups.count == 2)
    let testInfectionInput = """
    801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
    4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4
    """
    let testInfectionArmy = try Army(input: testInfectionInput)
    assert(testInfectionArmy.groups.count == 2)
    
    var testWar = War(immuneSystem: testImmuneArmy, infection: testInfectionArmy)
    print(testWar.immuneSystem)
    print(testWar.infection)
    testWar.fight()
    assert(testWar.immuneSystem.groups.count == 1)
    assert(testWar.infection.groups.count == 2)
    for _ in 0..<7 {
        testWar.fight()
    }
    assert(testWar.immuneSystem.groups.count == 0)
    assert(testWar.infection.groups.count == 2)
    
    var testBoostWar = War(immuneSystem: try Army(input: testImmuneInput), infection: try Army(input: testInfectionInput))
    testBoostWar.immuneSystem.boost = 1570
    while testBoostWar.immuneSystem.groups.count > 0 && testBoostWar.infection.groups.count > 0 {
        testBoostWar.fight()
    }
    print("At end of war, infection has \(testBoostWar.infection.groups.reduce(0, { $0 + $1.unitCount })) groups")
    print("At end of war, immune system has \(testBoostWar.immuneSystem.groups.reduce(0, { $0 + $1.unitCount })) groups")
    
    
} else {
    let fullImmuneInput = """
    1614 units each with 8016 hit points (immune to slashing; weak to radiation) with an attack that does 48 fire damage at initiative 9
    3730 units each with 5611 hit points (immune to bludgeoning; weak to fire) with an attack that does 14 radiation damage at initiative 16
    1627 units each with 9770 hit points (weak to cold) with an attack that does 55 fire damage at initiative 3
    4665 units each with 9782 hit points (weak to fire) with an attack that does 18 radiation damage at initiative 10
    281 units each with 5764 hit points (immune to fire; weak to radiation) with an attack that does 187 slashing damage at initiative 19
    524 units each with 9344 hit points with an attack that does 158 cold damage at initiative 15
    5013 units each with 9768 hit points with an attack that does 15 cold damage at initiative 14
    1143 units each with 1822 hit points (weak to radiation) with an attack that does 15 fire damage at initiative 18
    136 units each with 6830 hit points (weak to radiation) with an attack that does 420 slashing damage at initiative 7
    665 units each with 7973 hit points (weak to bludgeoning; immune to slashing) with an attack that does 119 fire damage at initiative 11
    """
    let immuneArmy = try Army(input: fullImmuneInput)
    let fullInfectionInput = """
    515 units each with 8712 hit points (immune to radiation; weak to slashing, fire) with an attack that does 30 cold damage at initiative 1
    5542 units each with 56769 hit points with an attack that does 16 bludgeoning damage at initiative 4
    1663 units each with 10437 hit points (immune to slashing, fire, radiation) with an attack that does 12 radiation damage at initiative 12
    574 units each with 50124 hit points (weak to slashing, radiation) with an attack that does 171 fire damage at initiative 8
    1190 units each with 10652 hit points with an attack that does 16 cold damage at initiative 17
    3446 units each with 23450 hit points with an attack that does 12 fire damage at initiative 5
    5887 units each with 14556 hit points (weak to slashing) with an attack that does 4 radiation damage at initiative 2
    1761 units each with 41839 hit points (weak to cold) with an attack that does 35 cold damage at initiative 20
    4194 units each with 16090 hit points (immune to fire; weak to slashing) with an attack that does 6 fire damage at initiative 6
    2127 units each with 27065 hit points (weak to cold, slashing) with an attack that does 24 slashing damage at initiative 13
    """
    let infectionArmy = try Army(input: fullImmuneInput)
    
    var partAWar = War(immuneSystem: immuneArmy, infection: infectionArmy)
    while partAWar.immuneSystem.groups.count > 0 && partAWar.infection.groups.count > 0 {
        partAWar.fight()
    }
    
    print("At end of war, infection has \(partAWar.infection.groups.reduce(0, { $0 + $1.unitCount })) groups")
    print("At end of war, immune system has \(partAWar.immuneSystem.groups.reduce(0, { $0 + $1.unitCount })) groups")
    
    for boost in 1..<118 {
        var war = War(immuneSystem: try Army(input: fullImmuneInput), infection: try Army(input: fullInfectionInput))
        
        war.immuneSystem.boost = boost
        func getGroupCounts(w: War) -> [UUID: Int] {
            var groupCounts: [UUID: Int] = [:]
            for army in [w.immuneSystem, w.infection] {
                for group in army.groups {
                    groupCounts[group.id] = group.unitCount
                }
            }
            return groupCounts
        }
        var lastGroupCounts: [UUID: Int] = getGroupCounts(w: war)
        
        while war.immuneSystem.groups.count > 0 && war.infection.groups.count > 0 {
            war.fight()
            let currGroupCounts = getGroupCounts(w: war)
            if currGroupCounts == lastGroupCounts {
                print("Stalemate!")
                break
            }
            lastGroupCounts = currGroupCounts
        }
        print("Boost: \(war.immuneSystem.boost)")
        print("At end of war, infection has \(war.infection.groups.reduce(0, { $0 + $1.unitCount })) groups")
        print("At end of war, immune system has \(war.immuneSystem.groups.reduce(0, { $0 + $1.unitCount })) groups")
        
        if war.immuneSystem.groups.count > 0 && war.infection.groups.count == 0 {
            print("Immune system won!")
            break
        }
    }
}
