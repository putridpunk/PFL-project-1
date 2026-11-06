-- | Local reconstruction of the parser-combinator module from Lecture 11.
--
-- The project only requires submitting the main file; in the Codex environment
-- the course's own Parsing.hs is available.  This copy exists only so that the
-- project can be compiled and tested locally.
module Parsing
  ( Parser
  , parse
  , failure
  , getc
  , satisfy
  , char
  , many
  , many1
  , (<|>)
  ) where

import Control.Applicative (Alternative (empty, (<|>)))

-- | A parser for values of type @a@.
newtype Parser a = P (String -> [(a, String)])

-- | Run a parser on an input string.
parse :: Parser a -> String -> [(a, String)]
parse (P f) txt = f txt

-- | A parser that always fails.
failure :: Parser a
failure = P (\_ -> [])

-- | A parser that returns the next character, or fails at end of input.
getc :: Parser Char
getc = P $ \s ->
  case s of
    [] -> []
    (x : xs) -> [(x, xs)]

instance Functor Parser where
  fmap f (P p) = P $ \s -> [(f a, s') | (a, s') <- p s]

instance Applicative Parser where
  pure x = P $ \s -> [(x, s)]
  P pf <*> P px = P $ \s ->
    [(f x, s'') | (f, s') <- pf s, (x, s'') <- px s']

instance Monad Parser where
  return = pure
  P p >>= f = P $ \s ->
    case p s of
      [] -> []
      ((v, s') : _) -> parse (f v) s'

instance Alternative Parser where
  empty = failure
  P p <|> P q = P $ \s ->
    case p s of
      [] -> q s
      (r : _) -> [r]

-- | Accept a character satisfying a predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  x <- getc
  if f x then return x else failure

-- | Accept a specific character.
char :: Char -> Parser Char
char x = satisfy (== x)

-- | Repeat a parser zero or more times.
many :: Parser a -> Parser [a]
many p = many1 p <|> return []

-- | Repeat a parser one or more times.
many1 :: Parser a -> Parser [a]
many1 p = do
  v <- p
  vs <- many p
  return (v : vs)
