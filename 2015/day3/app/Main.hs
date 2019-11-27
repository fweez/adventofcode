module Main where

import System.IO
import Control.Exception
import Lib
    
main :: IO ()
main = bracket (openFile "input.txt" ReadMode) hClose 
               (\h -> do 
                contents <- hGetContents h
                putStrLn ("Part 1: " ++ show (uniquePositions contents))
                putStrLn ("Part 2: " ++ show (twoSantasPositions contents))
                )
