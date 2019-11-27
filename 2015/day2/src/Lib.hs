module Lib
    ( paperSize, ribbonSize, strToDims
    ) where

split :: String -> Char -> [String]
split "" _ = []
split xs c = let (ys, zs) = break (== c) xs
             in  if null zs then [ys] else ys : split (tail zs) c

type Dims = (Int, Int, Int)
makeDims :: String -> Dims
makeDims s = (x, y, z)
    where 
        l = split s 'x'
        x = read (head l)
        y = read (head (tail l))
        z = read (head (tail (tail l)))

strToDims :: String -> [Dims]
strToDims cs = map makeDims (lines cs)

paperSize :: [Dims] -> Int
paperSize ds = sum $ map packageArea ds

ribbonSize :: [Dims] -> Int
ribbonSize ds = sum $ map packageRibbonLength ds

packageArea :: Dims -> Int
packageArea s = (2 * h) + (2 * w) + (2 * l) + slack
        where
            (x, y, z) = s
            h = x * y
            w = y * z
            l = x * z
            slack = min (min h w) l

packageRibbonLength :: Dims -> Int
packageRibbonLength s = minDiameter + volume
        where
           (x, y, z) = s
           a = (2 * x) + (2 * y)
           b = (2 * y) + (2 * z)
           c = (2 * x) + (2 * z)
           minDiameter = min a (min b c)
           volume = x * y * z
