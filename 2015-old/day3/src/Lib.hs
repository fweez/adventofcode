module Lib
    ( uniquePositions, twoSantasPositions
    ) where

import Data.List

type Position = (Int, Int)

visits :: String -> [Position]
visits = foldr nextPosition [(0, 0)]

uniquePositions :: String -> Int
uniquePositions = length . nub . visits

nextPosition :: Char -> [Position] -> [Position]
nextPosition c positions 
    | c == '^' = (x, y - 1): positions
    | c == '>' = (x + 1, y): positions
    | c == 'v' = (x, y + 1): positions
    | c == '<' = (x - 1, y): positions
    | otherwise = positions
    where
        (x, y) = head positions

twoSantasPositions :: String -> Int
twoSantasPositions s = 
    (length . nub) allHouses
    where 
        t = everyOther s
        u = everyOther . tail $ s
        santaPositions = visits t
        robotPositions = visits u
        allHouses = santaPositions ++ robotPositions

everyOther :: String -> String
everyOther "" = ""
everyOther (c:cs) = c: everyOther (drop 1 cs)

-- guesses:
-- 2130: too low
-- 2360: too high