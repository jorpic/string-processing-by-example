module StringExpr.Builder where

import Data.Text (Text)
import StringExpr.AST


class    GenIntExpr e where genIntExpr :: e -> IntExpr
instance GenIntExpr IntExpr where genIntExpr = id
instance GenIntExpr Int where genIntExpr = IntConst
instance GenIntExpr LoopVar where genIntExpr v = IntExpr 1 v 0

class    GenPos p where genPos :: p -> Pos
instance GenPos Pos where genPos = id
instance GenPos Int where genPos = CPos

class    GenToken p where genToken :: p -> Token
instance GenToken Token where genToken = id
instance GenToken CharClass where genToken = SomeOf

class    GenAtomicExp e where genAtomicExp :: e -> AtomicExpr
instance GenAtomicExp AtomicExpr where genAtomicExp = id
instance GenAtomicExp Text where genAtomicExp = ConstStr


eps :: RegExp
eps = []

pos :: (GenToken p, GenToken q, GenIntExpr c) => [p] -> [q] -> c -> Pos
pos p q c = Pos (map genToken p) (map genToken q) (genIntExpr c)

substr :: (GenPos a, GenPos b) => a -> b -> AtomicExpr
substr a b = SubStr (Input 0) (genPos a) (genPos b)

-- c-th occurence of r in the input string
substr2 :: (GenToken r, GenIntExpr c) => [r] -> c -> AtomicExpr
substr2 r c = substr (pos eps r c) (pos r eps c)

loop :: Int -> (LoopVar -> [AtomicExpr]) -> AtomicExpr
loop i f = let w = LoopVar i in Loop w $ Concat $ f w

inp :: Int -> AtomicExpr
inp i = SubStr (Input i) (CPos 0) (CPos (-1))

match :: GenToken t => Int -> [t] -> Match
match i rx = Match (Input i) (map genToken rx) 1

noMatch :: GenToken t => Int -> [t] -> Match
noMatch i rx = NoMatch (Input i) (map genToken rx) 1
