module Main where

import System.IO
import Control.Exception
import Lib

import Text.ParserCombinators.ReadP


main :: IO ()
main = bracket (openFile "input.txt" ReadMode) hClose 
                (\h -> do 
                contents <- hGetContents h
                let array = followLightingDirections contents
                putStrLn ("Part 1: " ++ show (sumLitLights array))
                putStrLn ("Part 2: " ++ show (sumBrightness array))
                )
