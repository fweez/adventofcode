import AOCShared
import Foundation
import Overture

typealias OrbitDictionary = [String.SubSequence: [String.SubSequence]]

var orbits: OrbitDictionary = [:]
func createOrbits(_ orbits: OrbitDictionary, _ line: String.SubSequence) -> OrbitDictionary {
    let bodies = line.split(separator: ")")
    guard bodies.count == 2 else { fatalError() }
    let orbited = bodies.first!
    let orbiter = bodies.last!
    var orbits = orbits
    orbits[orbited] = (orbits[orbited] ?? []) + [orbiter]
    return orbits
}
public func part1() {
    orbits = ingestFile("day6.txt")
        .reduce(OrbitDictionary(), createOrbits)
    print("Part 1: \(sumChildrenOf(orbits))") }
public func part2() { print("Part 2: \(sumOrbitalTransfers(orbits) ?? Int.min)") }

func sumChildrenOf(_ orbits: OrbitDictionary, _ orbited: String.SubSequence = "COM", _ parentOrbits: Int = 0) -> Int {
    (orbits[orbited] ?? [])
        .map { body -> Int in
            sumChildrenOf(orbits, body, parentOrbits + 1)
        }
        .reduce(parentOrbits, +)
}

func sumOrbitalTransfers(_ orbits: OrbitDictionary, _ orbited: String.SubSequence = "COM") -> Int? {
    orbits[orbited]
        .flatMap { children -> Int? in
            children
                .compactMap { child in distTo(orbits, child, "SAN").flatMap { (child, $0) } }
                .first
                .flatMap { santaChild, santaDist -> Int? in
                    distTo(orbits, santaChild, "YOU")
                        .map {
                            sumOrbitalTransfers(orbits, santaChild) ??
                            ($0 + santaDist) }
                }
            
        }
}

func distTo(_ orbits: OrbitDictionary, _ from: String.SubSequence, _ to: String.SubSequence) -> Int? {
    if orbits[from]?.contains(to) == true { return 0 }
    return orbits[from]
        .flatMap { children -> Int? in
            children
                .compactMap { child in distTo(orbits, child, to) }
                .first
                .map { $0 + 1 }
        }
}
