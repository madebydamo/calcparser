module Calc where

import Data.Text(pack, unpack, replace)
import Parsers

data Calc = Add Calc Calc | Mul Calc Calc | Sub Calc Calc | Div Calc Calc | Pow Calc Calc | Atom Double | Par Calc | Neg Calc deriving (Show)

atomP :: Parser Calc
atomP = do Atom <$> float

-- Presedence 1 -> +, -
pres1P :: Parser Calc
pres1P = pres2P ||| do
  p1 <- pres1P
  sign <- char '+' ||| char '-'
  p2 <- pres2P
  return (case sign of
    '+' -> Add p1 p2
    '-' -> Sub p1 p2
    _ -> error "unexpected operator")
  
-- Presedence 2 -> *, /
pres2P :: Parser Calc
pres2P = pres3P ||| do
  p1 <- pres3P
  sign <- char '*' ||| char '/'
  p2 <- pres2P
  return (case sign of
    '*' -> Mul p1 p2
    '/' -> Div p1 p2
    _ -> error "unexpected operator")

-- Presedence 3 -> ^
pres3P :: Parser Calc
pres3P = pres4P ||| do
  p1 <- pres4P
  sign <- char '^'
  p2 <- pres3P
  return (case sign of
    '^' -> Pow p1 p2
    _ -> error "unexpected operator")
  
-- Presedence 4 -> Atom, Parentesis, Minus sign
pres4P :: Parser Calc
pres4P = atomP ||| do
    open <- char '('
    p1 <- pres1P
    close <- char ')'
    return (Par p1)
  ||| 
  do
    neg <- char '-'
    Neg <$> pres4P

str2Calc :: String -> Calc
str2Calc = completeParse pres1P

foldCalc :: Calc -> Double
foldCalc (Add c1 c2) = foldCalc c1 + foldCalc c2
foldCalc (Mul c1 c2) = foldCalc c1 * foldCalc c2
foldCalc (Sub c1 c2) = foldCalc c1 - foldCalc c2
foldCalc (Div c1 c2) = foldCalc c1 / foldCalc c2
foldCalc (Pow c1 c2) = foldCalc c1 ** foldCalc c2
foldCalc (Par c) = foldCalc c
foldCalc (Neg f) = negate $ foldCalc f
foldCalc (Atom f) = f

removeWhiteSpace :: String -> String
removeWhiteSpace s1 = unpack (Data.Text.replace search replace input)
  where 
    search = pack " " 
    replace = pack ""
    input = pack s1

-- Input a string in the from of "3.4 + -34 * 2 ^ 0.3 - 2.2321 / 3"
-- and receive the result with correct order of operation
calc :: String -> Double
calc = foldCalc . str2Calc . removeWhiteSpace
