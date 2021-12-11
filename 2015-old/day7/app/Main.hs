module Main where

import System.IO
import Control.Exception
import Lib
import Data.Map (Map, findWithDefault)

main :: IO ()
main = bracket (openFile "test.txt" ReadMode) hClose 
                (\h -> do 
                contents <- hGetContents h
                let wires = followWiringInstructions contents
                putStrLn ("Part 1: " ++ show (extractKey "a" wires))
                --putStrLn ("Part 2: " ++ show (sumBrightness array))
                )