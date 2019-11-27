module Lib
    ( floorFinder, parenFinder
    ) where

floorFinder :: String -> Int
floorFinder = foldr parenValue 0

parenValue :: Char -> Int -> Int
parenValue c a | c == '(' = 1 + a
               | c == ')' = -1 + a
               | otherwise = -9999999

parenFinder :: String -> Int -> Int -> Int
parenFinder s count currFloor | currFloor == -1 = count
                              | otherwise = parenFinder (tail s) (count + 1) (parenValue (head s) currFloor)