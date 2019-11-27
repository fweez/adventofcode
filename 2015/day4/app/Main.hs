module Main where

import Lib

main :: IO ()
main = do
    putStrLn ("Part 1: " ++ show (mine isCoin5 "iwrupvqb"))
    putStrLn ("Part 2: " ++ show (mine isCoin6 "iwrupvqb"))
