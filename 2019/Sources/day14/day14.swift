import AOCShared
import Foundation
import Overture

public struct ChemicalMeasurement {
    let chemical: String
    let amount: Int
}

extension ChemicalMeasurement: Hashable { }

extension ChemicalMeasurement: CustomStringConvertible {
    public var description: String { "\(amount) \(chemical)" }
}

public typealias Recipe = (output: ChemicalMeasurement, input: [ChemicalMeasurement])

public typealias RecipeBook = [String: Recipe]

public func parse(_ fileName: String) -> RecipeBook {
    .init(uniqueKeysWithValues: parseFile(recipeParser, fileName))
}
public func part1(_ recipes: RecipeBook) { print("Part 1: \(produceFuel(recipes))") }
public func part2(_ recipes: RecipeBook) { print("Part 2: \(produceFuelUntilOreExhausted(recipes, available: ["ORE": 1000000000000]))") }

let chemMeasurementParser = zip(
    intParser,
    literal(" "),
    alphaParser
)
    .map { ChemicalMeasurement(chemical: String($0.2), amount: $0.0) }

let recipeParser = zip(
    zeroOrMore(chemMeasurementParser, separatedBy: literal(", ")),
    literal(" => "),
    chemMeasurementParser
)
    .map { t -> (String, Recipe) in (outputKind: t.2.chemical, recipe: (output: t.2, input: t.0)) }
    
typealias Reactions = (requirements: [ChemicalMeasurement], available: [String: Int])

func produceFuel(_ recipes: RecipeBook, availableOre: Int = Int.max) -> Int  {
    availableOre - ((satisfy(
        recipes: recipes,
        reactions: (requirements: [ChemicalMeasurement(chemical: "FUEL", amount: 1)],
                    available: ["ORE": availableOre])
        ) ?? (requirements: nil, available: [:]))
        .available["ORE"] ?? 0)
}

func produceFuelUntilOreExhausted(_ recipes: RecipeBook, available: [String: Int]) -> Int {
    var available = available
    var count = 0
    (0...7)
        .reversed()
        .map { Int(pow(10, Double($0))) }
        .forEach { fuelAmt in
            while true {
                guard let next = satisfy(
                    recipes: recipes,
                    reactions: (requirements: [ChemicalMeasurement(chemical: "FUEL", amount: fuelAmt)],
                                available: available)
                    ) else {
                        break
                }
                count += fuelAmt
                available = next.available
                if count % 1000 == 0 { print(count) }
            }
            print("Exhausted \(fuelAmt) search, count is \(count)")
        }
    
    return count
}


func satisfy(recipes: RecipeBook, reactions: Reactions) -> Reactions? {
    var nextRequirements = reactions
        .requirements
    
    guard nextRequirements.count > 0 else {
        return (requirements: compressed(reactions.requirements),
                available: reactions.available)
    }
    
    var (requirement, nextAvailable) = consumeAvailable(
        producing: nextRequirements.removeFirst(),
        available: reactions.available)
    guard let nextRequirement = requirement else {
        return satisfy(
            recipes: recipes,
            reactions: (
                requirements: nextRequirements,
                available: nextAvailable))
    }
    guard let requirementRecipe = recipes[nextRequirement.chemical] else {
        return nil
    }
    let copies = Int(ceil(Double(nextRequirement.amount) / Double(requirementRecipe.output.amount)))
    nextRequirements.append(contentsOf: requirementRecipe
        .input
        .map { m in
            ChemicalMeasurement(chemical: m.chemical, amount: m.amount * copies)
        })
    let remainder = (requirementRecipe.output.amount * copies) - nextRequirement.amount
    nextAvailable[nextRequirement.chemical] = (nextAvailable[nextRequirement.chemical] ?? 0) + remainder
    return satisfy(
        recipes: recipes,
        reactions: (requirements: compressed(nextRequirements), available: nextAvailable))
}

func consumeAvailable(producing: ChemicalMeasurement, available: [String: Int]) -> (producing: ChemicalMeasurement?, available: [String: Int]) {
    guard let availableAmount = available[producing.chemical] else {
        return (producing, available)
    }
    var available = available
    let remaining = availableAmount - producing.amount
    if remaining >= 0 {
        available[producing.chemical] = remaining
        return (nil, available)
    } else {
        available[producing.chemical] = 0
        return (ChemicalMeasurement(chemical: producing.chemical, amount: abs(remaining)), available)
    }
}

func compressed(_ measurements: [ChemicalMeasurement]) -> [ChemicalMeasurement] {
    Array(Set(measurements.map { $0.chemical })
        .map { chemical -> ChemicalMeasurement in
            .init(
                chemical: chemical,
                amount: measurements
                    .filter { $0.chemical == chemical }
                    .reduce(0, { sum, curr in sum + curr.amount }))
            })
        .sorted { a, b in a.chemical != "ORE" }
}
