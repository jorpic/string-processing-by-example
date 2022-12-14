module Eval (spec) where

import Test.Hspec
import StringExpr
import Utils

-- ADD MORE
-- i == SubStr i CPos(0) CPos(-1)
--
-- eps = [] :: RegExp -- matches AN EMPTY STRING

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
