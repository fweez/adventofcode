import AOCShared
import Foundation
import Overture

public func parse(_ fileName: String) -> [[Int]] { layers(ingestFile(fileName).first!) }
public func part1(_ layers: [[Int]]) { print("Part 1: \(getMagicNumber(layers))") }
public func part2(_ layers: [[Int]]) { print("Part 2: \n\(printableSIF(layers))") }

func getMagicNumber(_ layers: [[Int]]) -> Int {
    ///To make sure the image wasn't corrupted during transmission, the Elves would like you to find the layer that contains the fewest 0 digits. On that layer, what is the number of 1 digits multiplied by the number of 2 digits?
    let layer = layers
        .map(pipe({ $0.filter { $0 != 0 } },
                  { (a: [Int]) -> (Int, Int, Int) in (a.count, a.filter({ $0 == 1 }).count, a.filter({ $0 == 2 }).count) }))
        .sorted { a, b -> Bool in a.0 > b.0 }
        .first!
    return layer.1 * layer.2
}

func layers(_ s: String.SubSequence) -> [[Int]] {
    let input = s.map { Int(String($0))! }
    /// The image you received is 25 pixels wide and 6 pixels tall.
    return stride(from: 0, to: input.count, by: 6 * 25)
        .map { (a: Int) -> [Int] in Array(input[a..<(a + (6 * 25))]) }
}

func printableSIF(_ layers: [[Int]]) -> String {
    layers
        .reduce(Array(repeating: Array(repeating: 2, count: 25), count: 6), integrateLayer)
        .map(renderSIFRows)
        .joined(separator: "\n")
}

func integrateLayer(_ currentImage: [[Int]], _ layer: [Int]) -> [[Int]] {
    (0..<6)
        .map { y in
            (0..<25)
                .map { x in
                    let sifPx = layer[(y * 25) + x]
                    if sifPx != 2 && currentImage[y][x] == 2 {
                        return sifPx
                    } else {
                        return currentImage[y][x]
                    }
            }
    }
}

func renderSIFRows(_ row: [Int]) -> String {
    row
        .map { n in
            switch n {
            case 2: return " "
            case 1: return "."
            case 0: return "X"
            default: fatalError()
            }
    }.joined()
}
