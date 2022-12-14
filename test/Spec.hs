import Data.Text (Text)
import  Test.Hspec

import StringExpr

main :: IO ()
main = hspec spec

eval :: [Text] -> AtomicExpr -> Either EvalErr Text
eval t p = runEval (evalAtomic p) $ EvalContext t []

spec :: Spec
spec = do
  describe "substring with CPos" $ do
    let t = ["0123456789ABCDEF"]
    let p i j = SubStr (Input 0) (CPos i) (CPos j)
    it "just works"
      $ eval t (p    5   (-5)) `shouldBe` Right "56789AB"
    it "is ok with negative bounds"
      $ eval t (p (-16) (-10)) `shouldBe` Right "0123456"
    it "checks bounds"
      $ eval t (p (-25)    5)  `shouldBe` Left "CPos is out of range"
    it "returns empty on reverse bounds"
      $ eval t (p    1     0)  `shouldBe` Right ""
    it "gets one char"
      $ eval t (p  (-1)  (-1)) `shouldBe` Right "F"

  let ok = (`shouldBe` True)
  let no = (`shouldBe` False)
  describe "matchesPrefix with empty" $ do
    it "1" $ ok $ [] `matchesPrefix` ""
    it "2" $ ok $ [] `matchesPrefix` "any"

  describe "matchesPrefix with SomeOf" $ do
    let ex = [SomeOf AlphTok, SomeOf NumTok]
    it "1" $ ok $ ex `matchesPrefix` "a1+"
    it "2" $ ok $ ex `matchesPrefix` "ab12+"
    it "3" $ no $ ex `matchesPrefix` "a+++"
    it "4" $ no $ ex `matchesPrefix` "1a"
    it "5" $ no $ ex `matchesPrefix` "aa"
    it "6" $ no $ ex `matchesPrefix` ""

  describe "matchesPrefix with SomeNoneOf" $ do
    let ex = [SomeNotOf NumTok, SomeOf NumTok, SomeNotOf AlphTok]
    it "1" $ ok $ ex `matchesPrefix` "a1+"
    it "2" $ ok $ ex `matchesPrefix` "ab12+"
    it "3" $ no $ ex `matchesPrefix` "12a"
    it "4" $ no $ ex `matchesPrefix` "-12"
    it "5" $ no $ ex `matchesPrefix` "-12a"

  describe "example #2 from the paper" $ do
    let p = SubStr
            (Input 0)
            (Pos [] [SomeOf NumTok] (IntConst 0))
            (CPos (-1))
    it "1" $ eval ["BTR KRNL WK CORN 15Z"]      p `shouldBe` Right "15Z"
    it "2" $ eval ["CAMP DRY DBL NDL 3.6 OZ"]   p `shouldBe` Right "3.6 OZ"
    it "3" $ eval ["CHORE BOY HD SC SPNG 1 PK"] p `shouldBe` Right "1 PK"
    it "4" $ eval ["FRENCH WORCESTERSHIRE 5 Z"] p `shouldBe` Right "5 Z"
    it "5" $ eval ["O F TOMATO PASTE 6 OZ"]     p `shouldBe` Right "6 OZ"
