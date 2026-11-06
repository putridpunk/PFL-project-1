-- | Logical calculator for the first PFL project.
--
-- Each input line is parsed as a proposition and classified as a
-- tautology, a contradiction or a contingent proposition.
module Main (main) where

import Data.Char (isAsciiLower, isSpace)
import Parsing (Parser, char, many, parse, satisfy)

-- | Logical propositions.
data Prop
  = Const Bool     -- ^ constants (True/False)
  | Var Char       -- ^ variables
  | And Prop Prop  -- ^ conjunction
  | Or Prop Prop   -- ^ disjunction
  | Not Prop       -- ^ negation
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Parser for propositions
-- ---------------------------------------------------------------------------

-- The grammar, with conjunction binding more tightly than disjunction:
--
--   prop     ::= term propCont
--   propCont ::= '|' term propCont | epsilon
--   term     ::= factor termCont
--   termCont ::= '&' factor termCont | epsilon
--   factor   ::= '!' factor | name | constant | '(' prop ')'
--   constant ::= 'T' | 'F'
--
-- The uses of 'many' followed by 'foldl' mirror the calculator example
-- from Lecture 11 and make both operators left associative.

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

-- | Parse a complete input line, ignoring white space.
parseProp :: String -> Maybe Prop
parseProp input =
  case parse prop (filter (not . isSpace) input) of
    [(p, "")] -> Just p
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- Truth tables and classification
-- ---------------------------------------------------------------------------

-- | A truth assignment for the variables of a proposition.
type Subst = [(Char, Bool)]

-- | Evaluate a proposition under a truth assignment.
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) =
  case lookup x s of
    Just b -> b
    Nothing -> False -- unreachable: substs assigns every variable
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Or p q) = eval s p || eval s q

-- | All variables occurring in a proposition (without repetitions).
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q

-- | Remove duplicate elements, keeping the first occurrence.
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

-- | All possible truth assignments for @n@ variables.
assignments :: Int -> [[Bool]]
assignments 0 = [[]]
assignments n =
  [b : bs | b <- [False, True], bs <- assignments (n - 1)]

-- | All truth assignments for the variables of a proposition.
substs :: Prop -> [Subst]
substs p = map (zip vs) (assignments (length vs))
  where
    vs = rmdups (vars p)

-- | The truth table of a proposition: one row per assignment.
truthTable :: Prop -> [(Subst, Bool)]
truthTable p = [(s, eval s p) | s <- substs p]

-- | Classify a proposition using its truth table.
classify :: Prop -> String
classify p
  | all isTrue table = "TAUTOLOGY"
  | all isFalse table = "CONTRADICTION"
  | otherwise = "CONTINGENT"
  where
    table = truthTable p
    isTrue (_, b) = b
    isFalse (_, b) = not b

-- ---------------------------------------------------------------------------
-- Program
-- ---------------------------------------------------------------------------

classifyLine :: String -> String
classifyLine line =
  case parseProp line of
    Just p -> classify p
    Nothing -> "INVALID"

main :: IO ()
main =
  interact
    ( unlines
        . map classifyLine
        . filter (not . all isSpace)
        . lines
    )
