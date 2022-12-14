module Utils where

import Data.Text (Text)
import Test.Hspec
import StringExpr

eval :: [Text] -> AtomicExpr -> Either EvalErr Text
eval t e = runEval (evalAtomic e) t

checkAtom :: AtomicExpr -> [Text] -> Text -> Expectation
checkAtom e t r = eval t e `shouldBe` Right r

checkExpr :: Expr -> [Text] -> Text -> Expectation
checkExpr e t r = runEval (evalExpr e) t `shouldBe` Right r
