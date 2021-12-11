module Lib
    ( followWiringInstructions, parse, extractKey
    ) where

import Data.Map (Map, fromList, insert, findWithDefault)
import Text.ParserCombinators.ReadP
import Control.Applicative
import Data.Bits 
import Data.Word

type ValType = Data.Word.Word16

followWiringInstructions :: String -> Map String Operation
followWiringInstructions input =
    foldl followInstruction (fromList []) (lines input)
data Symbol
    = Key String
    | Val ValType
    deriving Show

data Operation
    = Set Symbol Symbol --  b = a
    | And Symbol Symbol Symbol -- c = a & b 
    | Or Symbol Symbol Symbol -- c = a | b
    | RShift Symbol Symbol Symbol -- c = a >> b
    | LShift Symbol Symbol Symbol -- c = a << b
    | Not Symbol Symbol -- !a = b
    deriving Show

followInstruction :: Map String Operation -> String -> Map String Operation
followInstruction wires input = 
    insert key operation wires
    where 
        operation = parse input
        key = 
            case operation of
                Set _ (Key k) -> k
                And _ _ (Key k) -> k
                Or _ _ (Key k) -> k
                RShift _ _ (Key k) -> k
                LShift _ _ (Key k) -> k
                Not _ (Key k) -> k

parse :: String -> Operation
parse input = 
    fst $ last $ readP_to_S operation input

-- input looks like
-- 123 -> x
-- 456 -> y
-- x AND y -> d
-- x OR y -> e
-- x LSHIFT 2 -> f
-- y RSHIFT 2 -> g
-- NOT x -> h
-- NOT y -> i

num :: ReadP Symbol
num = do
    n <- many1 (satisfy (`elem` "0123456789"))
    return $ Val $ read n

identifier :: ReadP Symbol
identifier = do
    s <- many1 (satisfy (\char -> char >= 'a' && char <= 'z'))
    return $ Key s

symbol :: ReadP Symbol
symbol = 
    num <|> identifier

arrow :: ReadP String
arrow = 
    string " -> "
    
setOp :: ReadP Operation
setOp = do
    a <- symbol
    arrow
    b <- symbol
    return $ Set a b

tripleOp :: String -> ReadP (Symbol, Symbol, Symbol)
tripleOp s = do
    a <- symbol
    string s
    b <- symbol
    arrow
    c <- identifier
    return (a, b, c)

andOp :: ReadP Operation
andOp = do
    (a, b, c) <- tripleOp " AND "
    return $ And a b c

orOp :: ReadP Operation
orOp = do
    (a, b, c) <- tripleOp " OR "
    return $ Or a b c

shiftOp :: String -> ReadP (Symbol, Symbol, Symbol)
shiftOp s = do
    a <- symbol
    string s
    b <- num
    arrow
    c <- identifier
    return (a, b, c)

lshiftOp :: ReadP Operation
lshiftOp = do
    (a, b, c) <- shiftOp " LSHIFT "
    return $ LShift a b c
    
rshiftOp :: ReadP Operation
rshiftOp = do
    (a, b, c) <- shiftOp " RSHIFT "
    return $ RShift a b c

notOp :: ReadP Operation
notOp = do
    string "NOT "
    a <- symbol
    arrow
    b <- identifier
    return $ Not a b

operation :: ReadP Operation
operation =
    setOp <|> andOp <|> orOp <|> lshiftOp <|>  rshiftOp <|> notOp

extractKey :: String -> Map String Operation -> ValType
extractKey k wires =
    extractValue (Key k) wires

extractValue :: Symbol -> Map String Operation -> ValType
extractValue symbol wires =
    case symbol of 
        Key a -> runOperation (findWithDefault (Set (Val 0) (Key a)) a wires) wires
        Val a -> a

runOperation :: Operation -> Map String Operation -> ValType
runOperation (Set a (Key b)) wires =
    extractValue a wires
runOperation (And a b (Key c)) wires =
    a' .&. b'
    where
        a' = extractValue a wires
        b' = extractValue b wires
runOperation (Or a b (Key c)) wires =
    a' .|. b'
    where
        a' = extractValue a wires
        b' = extractValue b wires
runOperation (LShift a (Val i) (Key c)) wires =
    rotateL a' (fromInteger (toInteger i))
    where
        a' = extractValue a wires
runOperation (RShift a (Val i) (Key c)) wires =
    rotateR a' (fromInteger (toInteger i))
    where
        a' = extractValue a wires
runOperation (Not a (Key c)) wires =
    complement a'
    where
        a' = extractValue a wires
