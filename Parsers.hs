module Parsers where

-- NOTE: The development of some combinator library for parsers is not
-- difficult per se. However, making a production quality combinator
-- library involves a lot of thinking and optimization. This has already
-- been done as part of the "Parsec" library. In a recent GHC distribution
-- this library can be used with
--
--   import Text.ParserCombinators.Parsec
--
-- For its documentation ask Hoogle with "Parsec"

import Data.Char 
import qualified Control.Monad

-- Parsers are encapsulated in a new datatype in order to 
-- guarantee type-safety (and to enable making it a Monad instance).
data Parser a = Prs (String -> [(a,String)])

-- Apply a parser to a string.
parse :: Parser a -> String -> [(a,String)]
parse (Prs p) inp = p inp

-- Apply a parser and return the value of the first complete parse.
-- NOTE: We could use Maybe to defer error handling
completeParse :: Parser a -> String -> a
completeParse p inp 
  | null results = error "Parse unsuccessful"
  | otherwise    = head results
  where results = [val | (val,[]) <- parse p inp]

-- In order to support the "do"-notation, we have to make
-- our Parser type an instance of the Monad typeclass.

instance Functor Parser where
  fmap = Control.Monad.liftM

instance Applicative Parser where
  pure x = Prs (\inp -> [(x,inp)])
  (<*>) = Control.Monad.ap

instance Monad Parser where
    -- return :: a -> Parser a 
    --
    -- Do not consume any input and return a fixed result.
    return x = Prs (\inp -> [(x,inp)])

    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    --
    -- Sequence a parser with a parser generator. The parser
    -- generator generates a parser depending on the outcome
    -- of the first parser. The generated parser is then applied
    -- to the input.
    p >>= gen = Prs (\inp -> [ (val',out') | (val,out) <- parse p inp, 
                                             (val',out') <- parse (gen val) out ])
    
    -- fail :: String -> Parser a
    --
    -- Every monad supports the method fail, which signals an error
    -- described by a string. As we cannot store an error value in
    -- our parser type, we just ignore it and signal a simple failure.
    -- fail = const failure

--------------------------------------------------------------------------

-- choice combinator
(|||) :: Parser a -> Parser a -> Parser a
p ||| q = Prs (\inp -> (parse p inp) ++ (parse q inp))

-- repetition
many :: Parser a -> Parser [a]    -- 0 or more repetitions of p
many p = many1 p ||| return []    

many1 :: Parser a -> Parser [a]
many1 p = do val <- p             -- 1 or more repetitions of p
             vals <- many p
             return (val:vals)

--------------------------------------------------------------------------
-- Simple parsers
--------------------------------------------------------------------------

-- The parser that always fails.
failure :: Parser a
failure = Prs (\_ -> []) 

item :: Parser Char
item = Prs (\inp -> case inp of
                    "" -> []
                    (x:xs) -> [(x,xs)])

sat  :: (Char -> Bool) -> Parser Char
sat p = do x <- item 
           if p x then return x else failure

char  :: Char -> Parser Char
char x = sat (==x)

string       :: String -> Parser String
string ""     = return ""
string (x:xs) = do char x 
                   string xs 
                   return (x:xs)

-- parsing integers
numPos :: Parser Int
numPos = do digits <- many1 (sat isDigit) 
            return (read digits)

numNeg :: Parser Int
numNeg = do char '-' 
            n <- numPos 
            return (-n)

num :: Parser Int
num = numPos ||| numNeg

floatPos :: Parser Double
floatPos = 
  do 
    pre <- many1 (sat isDigit)
    point <- char '.'
    post <- many1 (sat isDigit)
    return (read (pre ++ "." ++ post))
  |||
  do
    digits <- many1 (sat isDigit)
    return (read digits)

float :: Parser Double
float = floatPos

