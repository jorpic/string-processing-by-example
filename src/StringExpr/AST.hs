module StringExpr.AST where

import Data.Char (generalCategory, GeneralCategory(..), isAlpha, isDigit)


-- Regexps are deliberately limited in their expressiveness.
-- Disjunction is not allowed at all. And only restricted variat of Kleene
-- star is allowed − the one with more than zero matches. This makes it
-- possible to efficiently enumerate regular expressions that match a string.

type RegExp = [Token]
data Token
  = StartTok
  | EndTok
  | SomeOf CharClass
  | SomeNotOf CharClass
  deriving (Eq, Show)

-- Numeric Digits (0-9),
-- Alphabets (a-zA-Z), Lowercase alphabets (a-z), Uppercase alphabets (A-Z),
-- Accented alphabets, Alphanumeric characters,
-- Whitespace characters, All characters.
data CharClass
  = AlphTok | NumTok | NonDigitTok | HyphenTok
  | OtherTok GeneralCategory
  deriving (Eq, Show)

isCls :: CharClass -> Char -> Bool
isCls = \case
  AlphTok -> isAlpha
  NumTok -> isDigit
  NonDigitTok -> not . isDigit
  HyphenTok -> (== DashPunctuation) . generalCategory
  OtherTok c -> (== c) . generalCategory


data Pos
  = CPos Int
  | Pos RegExp RegExp Int
  deriving (Eq, Show)


data Expr
  = Input
  | SubStr Expr Pos Pos
  -- ^ SubStr(inp, CPos(0), CPos(-1)) == inp
  | SubStr2 Expr RegExp Int
  -- ^ SubStr(inp, Pos(ε, r, c), Pos(r, ε, c))
  -- c-th occurence of regex r
  deriving (Eq, Show)
