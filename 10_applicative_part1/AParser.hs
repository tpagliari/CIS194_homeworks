module AParser where

import Control.Applicative
import Data.Char
import Data.Maybe (isJust)

-- A parser for a value of type a is a function which takes a String
-- representing the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- Takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

-- parser for positive integers
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs


-- Homework

-- 1
-- Functor instance for parser
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g (Parser h) = Parser {runParser = first g . h} -- <$>

first :: (a -> b) -> Maybe (a,c) -> Maybe (b,c)
first g = fmap g'
  where g' (x, y) = (g x, y)

-- 2
-- Applicative instance for parser
instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser {runParser = f}
    where f xs = Just (a, xs)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- p1 is the parser that produces a function of type (a -> b)
  -- p2 is the parser that produces value of type a
  -- the result is the parser that produces value of type b
  -- do is syntactic sugar for the Maybe monad.
  (<*>) p1 p2 = Parser {runParser = f}
    where f str = do
            (g, str')  <- (runParser p1) str
            (x, str'') <- (runParser p2) str'
            return (g x, str'')

  --(<*>) (Parser p1) (Parser p2) = Parser f
  --    where f str = do 
  --            (g, str')  <- p1 str
  --            (x, str'') <- p2 str'
  --            return (g x, str'')

-- 3
-- A parser that expects to see chers ’a’ and ’b’ and returns them as a pair
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- A parser that acts in the same way as abParser but returns () instead of ’a’ and ’b’
abParser_ :: Parser ()
abParser_ = const () <$> abParser

-- A parser that reads two integer values separated by a space and returns the integer values in a list.
intPair :: Parser [Integer]
intPair = (\x y z -> [x, z]) <$> posInt <*> char ' ' <*> posInt

-- 4
-- Alternative instance for Parser
instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser f
    where f str = case runParser p1 str of
                    Just result -> Just result
                    Nothing -> runParser p2 str
                    
  -- using alternative instance for Maybe
  --p1 <|> p2 = Parser f
  --  where f str = runParser p1 str <|> runParser p2 str