var recipes = [3,7]
var e1idx = 0
var e2idx = 1

func appendRecipes() {
    let newvalue = recipes[e1idx] + recipes[e2idx]
    let tens: Int
    if newvalue > 9 {
        tens = 1
    } else {
        tens = 0
    }
    let ones = newvalue - (tens * 10)
    assert(ones < 10)
    if tens > 0 {
        recipes.append(tens)
    }
    recipes.append(ones)
}

func updateElves() {
    e1idx = (e1idx + recipes[e1idx] + 1) % recipes.count
    e2idx = (e2idx + recipes[e2idx] + 1) % recipes.count
}

for _ in 0..<306291 {
    appendRecipes()
    updateElves()
}
//After 5 recipes, the scores of the next ten would be 0124515891.
assert(recipes[5..<15] == [0,1,2,4,5,1,5,8,9,1])
// After 9, 5158916779
assert(recipes[9..<19] == [5,1,5,8,9,1,6,7,7,9])
// After 18 recipes, the scores of the next ten would be 9251071085.
assert(recipes[18..<28] == [9,2,5,1,0,7,1,0,8,5])
// After 2018 recipes, the scores of the next ten would be 5941429882.
assert(recipes[2018..<2028] == [5,9,4,1,4,2,9,8,8,2])
print(recipes[306281..<306291])
