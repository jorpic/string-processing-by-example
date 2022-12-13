module StringExpr.AST where

import Data.Char (generalCategory, GeneralCategory(..), isAlpha, isDigit)
import Data.Text (Text)


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
  | Pos RegExp RegExp IntExpr

newtype StringExpr = Switch [(Predicate, TraceExpr)]
type Predicate = [[Match]] -- disjunction of conjunctions

data IntExpr
  = IntConst Int
  | IntExpr Int LoopVar Int -- k1 * var + k2

data Match
  = Match Input RegExp Int
  | NotMatch Input RegExp Int

newtype TraceExpr = Concat [AtomicExpr]

newtype Input = Input Int
newtype LoopVar = LoopVar Int

data AtomicExpr
  = ConstStr Text
  | SubStr Input Pos Pos
  -- ^ SubStr(inp, CPos(0), CPos(-1)) == inp
  | Loop LoopVar TraceExpr
