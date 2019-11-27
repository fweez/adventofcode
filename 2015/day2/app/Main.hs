module Main where

import System.IO
import Control.Exception
import Lib

main :: IO ()
main = bracket (openFile "input.txt" ReadMode) hClose 
               (\h -> do 
                contents <- hGetContents h
                let dims = strToDims contents
                putStrLn ("Part 1: " ++ show (paperSize dims))
                putStrLn ("Part 2: " ++ show (ribbonSize dims)))