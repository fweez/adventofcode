import AOCShared
import Foundation
import Overture

guard let inputFile = Bundle.module.url(forResource: "input", withExtension: "txt"),
      let input = try? String(contentsOf: inputFile) else {
    fatalError("Could not get contents of input file")
}

struct Food {
    let ingredients: Set<String>
    let allergens: Set<String>
}

typealias ParsedStructure = [Food]

func parse(_ input: String) -> ParsedStructure {
    let word = alphaParser.map(String.init)
    let ingredients = zeroOrMore(word, separatedBy: literal(" "))
    let allergens = zip(
        literal(" (contains "),
        zeroOrMore(word, separatedBy: literal(", ")),
        literal(")"))
        .map { _, w, _ in w }
    let food = zip(ingredients, allergens)
        .map { Food(ingredients: Set($0), allergens: Set($1)) }
    
    return input
        .split(separator: "\n")
        .compactMap(pipe(String.init, food.runStatic))
}

func findAllergenic(_ foods: [Food]) -> [String: Set<String>] {
    foods
        .reduce([String: Set<String>]()) { allergens, food in
            food
                .allergens
                .reduce(into: allergens) { maybe, allergen in
                    maybe[allergen] = maybe[allergen]?.intersection(food.ingredients) ?? Set(food.ingredients)
                }
        }
}

func findNonAllergenic(_ foods: [Food]) -> Int {
    let allergenic = findAllergenic(foods)
        .values
        .reduce(Set<String>()) { a, s in a.union(s) }
    let nonallergenic = foods
        .reduce([]) { accum, food in
            accum + [food.ingredients.subtracting(allergenic)]
        }
    let counts = nonallergenic
        .reduce(into: [String: Int]()) { accum, ingredients in
            ingredients
                .forEach { ingredient in
                    accum[ingredient] = (accum[ingredient] ?? 0) + 1
                }
        }
    return counts.values
        .reduce(0, +)
}

func part1(_ parsedInput: ParsedStructure) {
    let count = findNonAllergenic(parsedInput)
    guard count < 2780 else { fatalError() }
    print("Part 1: \(findNonAllergenic(parsedInput))")
}

func uniquify(allergens: [String: Set<String>]) -> [String: Set<String>] {
    if allergens.values.allSatisfy({ $0.count == 1 }) { return allergens }
    let uniques = allergens.values
        .filter { $0.count == 1 }
        .flatMap { $0 }
    let uniqued = allergens.map { t -> (String, Set<String>) in
        if t.value.count > 1 {
            return (t.key, t.value.subtracting(uniques))
        } else {
            return (t.key, t.value)
        }
    }
    let nextAllergens = Dictionary(uniqueKeysWithValues: uniqued)
    return uniquify(allergens: nextAllergens)
}

func p2Format(allergens: [String: Set<String>]) -> String {
    allergens
        .sorted { a, b in a.key < b.key }
        .map { $1.first! }
        .joined(separator: ",")
}

func part2(_ parsedInput: ParsedStructure) {
    let output = pipe(findAllergenic, uniquify, p2Format)(parsedInput)
    print("Part 2: \(output)")
}

func test1() {
    let testData = parse("""
            mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
            trh fvjkl sbzzf mxmxvkd (contains dairy)
            sqjhc fvjkl (contains soy)
            sqjhc mxmxvkd sbzzf (contains fish)
            """)
    assert(findNonAllergenic(testData) == 5)
}

func test2() {
    let testData = parse("""
            mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
            trh fvjkl sbzzf mxmxvkd (contains dairy)
            sqjhc fvjkl (contains soy)
            sqjhc mxmxvkd sbzzf (contains fish)
            """)
    let output = pipe(findAllergenic, uniquify, p2Format)(testData)
    assert(output == "mxmxvkd,sqjhc,fvjkl")
}

test1()
test2()
run(input: input, parse, part1, part2)

