module Lib
    ( followLightingDirections, sumLitLights, sumBrightness
    ) where

import Text.ParserCombinators.ReadP
import Control.Applicative

followLightingDirections :: String -> [[Int]]
followLightingDirections input =
    foldl manipulateLights initialArray (lines input)
    where
        initialArray = replicate 1000 $ replicate 1000 0

sumLitLights :: [[Int]] -> Int
sumLitLights = sum . map (sum . map (\b -> if b > 0 then 1 else 0))

sumBrightness :: [[Int]] -> Int
sumBrightness = sum . map sum

-- Input looks like
-- turn on 0,0 through 999,999
-- toggle 0,0 through 999,0
-- turn off 499,499 through 500,500

data Operation 
    = On
    | Off
    | Toggle
    deriving Show

type Point = (Int, Int)

num :: ReadP Int
num = do
    n <- many1 (satisfy (`elem` "0123456789"))
    return $ read n

point :: ReadP Point
point = do
    x <- num
    satisfy (== ',')
    y <- num
    return (x, y)

command :: ReadP (Operation, Point, Point)
command = do
    cmd <- on <|> toggle <|> off
    satisfy (== ' ')
    p1 <- point
    string " through "
    p2 <- point
    return (cmd, p1, p2)

on :: ReadP Operation
on = do
    string "turn on"
    return On

toggle :: ReadP Operation
toggle = do
    string "toggle"
    return Toggle
    
off :: ReadP Operation
off = do
    string "turn off"
    return Off

manipulateLights :: [[Int]] -> String -> [[Int]]
manipulateLights array input = 
    applyLightingOperationArray operation start end array
    where
        (operation, start, end) = fst $ last $ readP_to_S command input

applyLightingOperationArray :: Operation -> Point -> Point -> [[Int]] -> [[Int]]
applyLightingOperationArray op (startX, startY) (endX, endY) lightingArray =
    startRows ++ manipulatedRows ++ endRows
    where
        (startRows, rem) = splitAt startY lightingArray
        (_, endRows) = splitAt (endY + 1) lightingArray
        manipulatedRows = map (applyLightingOpRow op startX endX) $ fst $ splitAt ((endY + 1) - startY) rem

applyLightingOpRow :: Operation -> Int -> Int -> [Int] -> [Int]
applyLightingOpRow op startX endX lights =
    startLights ++ manipulatedLights ++ endLights
    where
        (startLights, rem) = splitAt startX lights
        (_, endLights) = splitAt (endX + 1) lights
        manipulatedLights = map (applyLightingOp op) $ fst $ splitAt ((endX + 1) - startX) rem

applyLightingOp :: Operation -> Int -> Int
applyLightingOp On i = i + 1
applyLightingOp Off i
    | i >= 1 = i - 1
    | otherwise = 0
applyLightingOp Toggle i = i + 2
