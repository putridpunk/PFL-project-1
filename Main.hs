-- PFL 2026/27 - Projeto 1
-- Logical calculator: reads propositions from stdin and classifies each one
-- as TAUTOLOGY, CONTRADICTION or CONTINGENT.
module Main where

-- you shouldn't submit the Parsing module 
-- (but you should keep be import below)
import Parsing

import Data.Char (isAsciiLower, isSpace)

-- AST for propositional formulas
data Prop
  = Const Bool     -- T / F
  | Var Char       -- variables are single lowercase letters
  | And Prop Prop  -- p & q
  | Or Prop Prop   -- p | q
  | Not Prop       -- !p
  deriving (Eq, Show)

-- Parser
-- ------

-- Grammar from the statement. & binds tighter than |, and both operators
-- are left associative:
--
--   prop   ::= term ('|' term)*
--   term   ::= factor ('&' factor)*
--   factor ::= '!' factor | var | 'T' | 'F' | '(' prop ')'
--
-- I'm using many + foldl like the calculator example from lecture 11.

prop :: Parser Prop
prop = do
  t <- term
  ts <- many (do _ <- char '|'; term)
  return (foldl Or t ts)

term :: Parser Prop
term = do
  f <- factor
  fs <- many (do _ <- char '&'; factor)
  return (foldl And f fs)

-- dispatch on the first char; variables are lowercase, so the last case is safe
factor :: Parser Prop
factor = do
  c <- satisfy isFactorStart
  case c of
    '!' -> do
      p <- factor
      return (Not p)
    '(' -> do
      p <- prop
      _ <- char ')'
      return p
    'T' -> return (Const True)
    'F' -> return (Const False)
    _ -> return (Var c)
  where
    isFactorStart c = isAsciiLower c || c `elem` "!(TF"

-- parse the whole line; if there's leftover input, it's invalid
parseProp :: String -> Maybe Prop
parseProp input =
  case parse prop (filter (not . isSpace) input) of
    [(p, "")] -> Just p
    _ -> Nothing

-- Truth tables
-- ------------

type Subst = [(Char, Bool)]

-- evaluate a formula under one assignment
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) =
  case lookup x s of
    Just b -> b
    Nothing -> False -- should never happen: substs gives values to all vars
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Or p q) = eval s p || eval s q

-- variables in the formula (duplicates possible)
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q

-- remove duplicates, keeping the first occurrence
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

-- all assignments for n variables (2^n lists)
assignments :: Int -> [[Bool]]
assignments 0 = [[]]
assignments n =
  [b : bs | b <- [False, True], bs <- assignments (n - 1)]

-- all assignments for the variables that actually occur in the formula
substs :: Prop -> [Subst]
substs p = map (zip vs) (assignments (length vs))
  where
    vs = rmdups (vars p)

-- one row per assignment
truthTable :: Prop -> [(Subst, Bool)]
truthTable p = [(s, eval s p) | s <- substs p]

-- all True => tautology, all False => contradiction, otherwise contingent
classify :: Prop -> String
classify p
  | all isTrue table = "TAUTOLOGY"
  | all isFalse table = "CONTRADICTION"
  | otherwise = "CONTINGENT"
  where
    table = truthTable p
    isTrue (_, b) = b
    isFalse (_, b) = not b

-- Program
-- -------

-- the statement doesn't say what to print for invalid input, so INVALID
classifyLine :: String -> String
classifyLine line =
  case parseProp line of
    Just p -> classify p
    Nothing -> "INVALID"

-- stdin -> stdout, one result per line; skip blank lines
main :: IO ()
main =
  interact
    ( unlines
        . map classifyLine
        . filter (not . all isSpace)
        . lines
    )
