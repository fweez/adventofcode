module Lib
    ( mine, isCoin5, isCoin6
    ) where

import Crypto.Hash
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Builder

mine :: (String -> Bool) -> String -> Int
mine isCoin s = checkHash isCoin s 0

checkHash :: (String -> Bool) -> String -> Int -> Int
checkHash isCoin s i = 
    if isCoin $ md5hash $ createInput s i
        then i
        else checkHash isCoin s (i + 1)

isCoin5 :: String -> Bool
isCoin5 digest = 
    take 5 digest == "00000"

isCoin6 :: String -> Bool
isCoin6 digest = 
        take 6 digest == "000000"
    

md5hash :: ByteString -> String
md5hash s = show (hash s :: Digest MD5)

createInput :: String -> Int -> ByteString
createInput s i = 
    pack (s ++ show i)