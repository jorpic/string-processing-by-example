module StringExpr.AST where

import Data.Char
  ( generalCategory
  , GeneralCategory(..)
  , isAlpha, isDigit, isUpper, isPrint
  )
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

data CharClass
  = AlphTok | NumTok | NonDigitTok | HyphenTok | DotTok
  | SlashTok | LeftParenTok | RightParenTok | UpperTok
  | SpaceTok | NonSpaceTok | CharTok
  | OtherTok GeneralCategory
  deriving (Eq, Show)

isCls :: CharClass -> Char -> Bool
isCls = \case
  AlphTok -> isAlpha
  NumTok -> isDigit
  NonDigitTok -> not . isDigit
  HyphenTok -> (== DashPunctuation) . generalCategory
  DotTok -> (== '.')
  SpaceTok -> (== Space) . generalCategory
  NonSpaceTok -> (/= Space) . generalCategory
  CharTok -> isPrint
  SlashTok -> (== '/')
  LeftParenTok -> (== '(')
  RightParenTok -> (== ')')
  UpperTok -> isUpper
  OtherTok c -> (== c) . generalCategory

data Pos
  = CPos Int
  | Pos RegExp RegExp IntExpr

newtype Expr = Switch [(Predicate, TraceExpr)]
type Predicate = [[Match]] -- disjunction of conjunctions

data IntExpr
  = IntConst Int
  | IntExpr Int LoopVar Int -- k1 * var + k2

data Match
  = Match Input RegExp Int
  | NoMatch Input RegExp Int

newtype TraceExpr = Concat [AtomicExpr]

newtype Input = Input Int
newtype LoopVar = LoopVar Int

data AtomicExpr
  = ConstStr Text
  | SubStr Input Pos Pos
  -- SubStr(inp, CPos(0), CPos(-1)) == inp
  -- SubStr2 inp r c := SubStr(inp, Pos(ε, r, c), Pos(r, ε, c))
  | Loop LoopVar TraceExpr
