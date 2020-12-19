import AOCShared
import Foundation
import Overture

let input = """
..#..#..
.###..#.
#..##.#.
#.#.#.#.
.#..###.
.....#..
#...####
##....#.
"""

let dimensionSize = 64

run(input: input, { _ in "" }, part1, part2)

typealias ParsedStructure = String

protocol PointN: Hashable {
    init(x: Int, y: Int)
    var neighbors: Set<Self> { get }
}

struct Point3: PointN {
    let x: Int
    let y: Int
    var z: Int = 0
    
    init(x: Int, y: Int) {
        self.x = x
        self.y = y
    }
    
    var neighbors: Set<Point3> {
        ((x - 1)...(x + 1))
            .reduce(into: Set<Point3>()) { set, x in
                ((y - 1)...(y + 1))
                    .forEach { y in
                        ((z - 1)...(z + 1))
                            .forEach { z in
                                if x == self.x,
                                   y == self.y,
                                   z == self.z { return }
                                var p = Point3(x: x, y: y)
                                p.z = z
                                set.insert(p)
                            }
                    }
            }
    }
}

struct Point4: PointN {
    let x: Int
    let y: Int
    var z: Int = 0
    var w: Int = 0
    
    init(x: Int, y: Int) {
        self.x = x
        self.y = y
    }
    
    var neighbors: Set<Point4> {
        ((x - 1)...(x + 1))
            .reduce(into: Set<Point4>()) { set, x in
                ((y - 1)...(y + 1))
                    .forEach { y in
                        ((z - 1)...(z + 1))
                            .forEach { z in
                                ((w - 1)...(w + 1))
                                    .forEach { w in
                                        if x == self.x,
                                           y == self.y,
                                           z == self.z,
                                           w == self.w { return }
                                        var p = Point4(x: x, y: y)
                                        p.z = z
                                        p.w = w
                                        set.insert(p)
                                    }
                            }
                    }
            }
    }
}

func parse<P>(_ input: String) -> Set<P> where P: PointN {
    input
        .split(separator: "\n")
        .enumerated()
        .reduce(Set<P>()) { set, t in
            set.union(
                t.element
                    .enumerated()
                    .compactMap { x, c in
                        guard c == "#" else { return nil }
                        return P(x: x, y: t.offset)
                    })
        }
}

func cycle<P>(_ dimension: Set<P>) -> Set<P> where P: PointN {
    let candidates = dimension
        .reduce(dimension) { candidates, point in candidates.union(point.neighbors) }
    return candidates
        .reduce(into: Set<P>()) { new, candidate in
            let filledNeighbors = candidate.neighbors.intersection(dimension)
            switch (filledNeighbors.count, dimension.contains(candidate)) {
            case (2, true),
                 (3, true),
                 (3, false): new.insert(candidate)
            default: return
            }
        }
}

func part1(_: ParsedStructure) {
    let finalDimension = (0..<6)
        .reduce(parse(input)) { (dim: Set<Point3>, cycleNumber: Int) -> Set<Point3> in
            print("Starting cycle \(cycleNumber)")
            let next = cycle(dim)
            return next
        }
    print("Part 1: \(finalDimension.count)")
}

func part2(_: ParsedStructure) {
    let finalDimension = (0..<6)
        .reduce(parse(input)) { (dim: Set<Point4>, cycleNumber: Int) -> Set<Point4> in
            print("Starting cycle \(cycleNumber)")
            let next = cycle(dim)
            return next
        }
    print("Part 2: \(finalDimension.count)")
}
