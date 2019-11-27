module Lib
    ( niceCount, newNiceCount
    ) where

niceCount :: String -> Int
niceCount input = sum $ map isNice (lines input)

newNiceCount :: String -> Int
newNiceCount input = sum $ map newIsNice (lines input)

type Niceness = (Int, Int, Int)
isNice :: String -> Int
isNice input = 
    if disqualifications == 0 && vowelCount >= 3 && dupes > 0 
        then 1
        else 0
    where
        (vowelCount, dupes, disqualifications) = calculateNiceness (0,0,0) input

newIsNice :: String -> Int
newIsNice input =
    if hasDupedPairs input && splitRepeats input
        then 1
        else 0


calculateNiceness :: Niceness -> String -> Niceness
calculateNiceness n "" = n
calculateNiceness (vowelCount, dupes, disqualifications) (c:cs)
    | cs == "" = (vowelCount + newVowelCount, dupes, disqualifications)
    | otherwise = calculateNiceness (vowelCount + newVowelCount, dupes + newDupes, disqualifications + newDisquals) cs
    where
        newVowelCount = checkVowel c
        next = head cs
        newDupes = checkDupes c next
        newDisquals = checkDisquals (c: [next])

hasDupedPairs :: String -> Bool
hasDupedPairs "" = False
hasDupedPairs input 
    | length input < 2 = False 
    | containsPair (take 2 input) (tail $ tail input) = True 
    | otherwise = hasDupedPairs $ tail input

containsPair :: String -> String -> Bool
containsPair _ "" = False
containsPair pair (c:cs) 
    | length nextTwo /= 2 = False
    | nextTwo == pair = True
    | otherwise = containsPair pair cs
    where
        nextTwo = c: take 1 cs

splitRepeats :: String -> Bool
splitRepeats input 
    | length triple /= 3 = False
    | head triple == last triple = True 
    | otherwise = splitRepeats $ tail input
    where
        triple = take 3 input

checkVowel :: Char -> Int
checkVowel 'a' = 1
checkVowel 'e' = 1
checkVowel 'i' = 1
checkVowel 'o' = 1
checkVowel 'u' = 1
checkVowel _ = 0

checkDupes :: Char -> Char -> Int
checkDupes x y 
    | x == y = 1
    | otherwise = 0

checkDisquals :: String -> Int
checkDisquals "ab" = 1
checkDisquals "cd" = 1
checkDisquals "pq" = 1
checkDisquals "xy" = 1
checkDisquals _ = 0

