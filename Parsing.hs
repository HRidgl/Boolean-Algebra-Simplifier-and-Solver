-- this file is responsible for parsing the user input string into expression form


-- Stating the name of the module so that it can be imported later
module Parsing where


-- Importing the relevant modules so that I can access additional functions
import Control.Applicative hiding (many)
import Control.Monad
import Data.Char

-- Expression and Op data types ( can be imported directly from Expression)
----------------------------------

import Expression

-- The monad of parsers
--------------------------

-- Creating a new Parser type so that I can return Parsers from functions
newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
  fmap f p = do
    p' <- p
    return (f p')

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  f <*> a = do
    f' <- f
    a' <- a
    return (f' a')

instance Monad Parser where
  return = pure
  p >>= f =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> parse (f v) out
      )

instance Alternative Parser where
  empty = mzero
  p <|> q = p ||| q

instance MonadPlus Parser where
  mzero = P (const [])
  p `mplus` q =
    P
      ( \inp -> case parse p inp of
          [] -> parse q inp
          [(v, out)] -> [(v, out)]
      )



-- Basic parsers
------------------

-- This parser always fails
failure :: Parser a
failure = mzero

-- This parser returns the first character if there is at least one character, or the empty list if an empty list is provided
item :: Parser Char
item =
  P
    ( \inp -> case inp of
        [] -> []
        (x : xs) -> [(x, xs)]
    )

-- This function allows you to call the different parsers from a universal function
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

-- This function allows you to apply 2 parsers in the order parser p then when parser p fails parser q will be used
(|||) :: Parser a -> Parser a -> Parser a
p ||| q = p `mplus` q



-- Derived primitives
-----------------------

-- This function parses only if a condition is satisfied else failure is returned
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else failure

-- Parses only if the first character is a digit
digit :: Parser Char
digit = sat isDigit

-- Only parses if the character provided matches a specified character
char :: Char -> Parser Char
char x = sat (== x)

-- Only parses if a string is provided
string :: String -> Parser String
string [] = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

-- Allows a parser to be applied zero or more times
many :: Parser a -> Parser [a]
many p = many1 p ||| return []

-- Allows a parser to be applied one or more times
many1 :: Parser a -> Parser [a]
many1 p = do
  v <- p
  vs <- many p
  return (v : vs)

-- Only parses natural numbers and returns an expression witht he value inside
nat :: Parser Expr
nat = do
  xs <- many1 (token digit)
  return (Val(read xs))



-- Allowing for negative numbers
-----------------------------------

-- Parses integers both posiitve and negative into expressions
int :: Parser Expr
int =
  do
    char '-'
    n <- nat
    return (makeNegative n)
    ||| nat

-- Makes a negative number negative by negating the value n inside
makeNegative :: Expr -> Expr
makeNegative (Val n) = Val (-n)
makeNegative n = n



-- Ignoring spacing
----------------------

-- Removes spaces by returning unit
space :: Parser ()
space = do
  many (sat isSpace)
  return ()

-- Removes all spaces from user input
token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

-- Removes all spaces from a symbol 
symbol :: String -> Parser String
symbol xs = token (string xs)

-- Removes all integers from around an integer
integer :: Parser Expr
integer = token int



-- Parsing expressions
-------------------------

-- Only parses if the input is valid else the value 0 is returned
cleanExpressionParser :: String -> Either String Expr
cleanExpressionParser a = 
  if validParse a
  then case parse expr a of
    [(parsedExpr, "")] -> Right parsedExpr
    [(partial, rest)] -> Left ("Parse error: got stuck at '" ++ rest ++ "'")
    [] -> Left "Parse error: invalid expression"
  else
    Left "Parse error: Invalid characters used"


-- This function checks that only numbers, operators, parenthesese and whitespace are in the user input expression
validParse :: String -> Bool
validParse [] = True
validParse (x:xs) | isDigit x   = validParse xs
                  | x == '+'    = validParse xs
                  | x == '-'    = validParse xs
                  | x == '*'    = validParse xs
                  | x == '/'    = validParse xs
                  | isSpace x   = validParse xs
                  | x == '('    = validParse xs
                  | x == ')'    = validParse xs
                  | x == '^'    = validParse xs
                  | x == '%'    = validParse xs
                  | otherwise   = False


-- This function returns the desired Op for the higher precedence operators
higherPrecedenceOperators :: Parser Op
higherPrecedenceOperators = (do
                                symbol "^"
                                return Pow)
                            |||
                            (do
                                symbol "%"
                                return Mod)

-- This function returns the desired Op for the medium precedence operators
mediumPrecedenceOperators :: Parser Op
mediumPrecedenceOperators = (do
                                symbol "*"
                                return Mul)
                            |||
                            (do
                                symbol "/"
                                return Div)

-- This function returns the desired Op for the lower precedence operators
lowerPrecedenceOperators :: Parser Op
lowerPrecedenceOperators = (do
                                symbol "+"
                                return Add)
                            |||
                            (do
                                symbol "-"
                                return Sub)


-- This method builds factors out of either a single natural number or a parenthesised expression
factor :: Parser Expr
factor = integer
        ||| do
                symbol "("
                e1 <- expr
                symbol ")"
                return e1

-- This function builds special expressions from many natural numbers
specialExpr :: Parser Expr
specialExpr = do
          f1 <- factor
          rest <- many (do
                          op <- higherPrecedenceOperators
                          f2 <- factor
                          return (op, f2))
          return (foldl (\acc (op, f2) -> Calc op acc f2) f1 rest)

-- This function builds terms from many special expressions
term :: Parser Expr
term = do
          s1 <- specialExpr
          rest <- many (do
                          op <- mediumPrecedenceOperators
                          s2 <- specialExpr
                          return (op, s2))
          return (foldl (\acc (op, s2) -> Calc op acc s2) s1 rest)

-- This function builds expressions from many terms
expr :: Parser Expr
expr = do
        t1 <- term
        rest <- many (do
                        op <- lowerPrecedenceOperators
                        t2 <- term
                        return (op, t2))
        return (foldl (\acc (op, t2) -> Calc op acc t2) t1 rest)

--------------------------
-- Level Loader Parsing --
--------------------------

-- Parser that returns a raw int
natInt :: Parser Int
natInt = do
  xs <- many1 (sat isDigit)
  return (read xs)

-- Parses an operator character into the Op type
opParser :: Parser Op
opParser = (char '+' >> return Add)
        ||| (char '-' >> return Sub)
        ||| (char '*' >> return Mul)
        ||| (char '/' >> return Div)
        ||| (char '^' >> return Pow)
        ||| (char '%' >> return Mod)

-- Parses a list separated by a comma
sepByComma :: Parser a -> Parser [a]
sepByComma p = do
  v <- p
  vs <- many (do { char ','; p })
  return (v:vs)