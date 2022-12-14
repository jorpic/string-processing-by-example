import Data.Text (Text)
import  Test.Hspec

import StringExpr
import StringExpr.Builder

main :: IO ()
main = hspec spec

eval :: [Text] -> AtomicExpr -> Either EvalErr Text
eval t p = runEval (evalAtomic p) t

checkEval :: AtomicExpr -> [Text] -> Text -> Expectation
checkEval p t r = eval t p `shouldBe` Right r

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
    let p = substr (pos eps [NumTok] (0 :: Int)) (-1 :: Int)
    it "1" $ checkEval p ["BTR KRNL WK CORN 15Z"]      "15Z"
    it "2" $ checkEval p ["CAMP DRY DBL NDL 3.6 OZ"]   "3.6 OZ"
    it "3" $ checkEval p ["CHORE BOY HD SC SPNG 1 PK"] "1 PK"
    it "4" $ checkEval p ["FRENCH WORCESTERSHIRE 5 Z"] "5 Z"
    it "5" $ checkEval p ["O F TOMATO PASTE 6 OZ"]     "6 OZ"

  describe "example #3 from the paper" $ do
    let p = substr (0 :: Int) (pos [SlashTok] eps (-1 :: Int))
    it "1" $ checkEval p ["Company/Code/index.html"]     "Company/Code/"
    it "2" $ checkEval p ["Company/Docs/Spec/specs.doc"] "Company/Docs/Spec/"

  describe "example #4 from the paper" $ do
    let p = loop 0 $ \w -> [substr2 [UpperTok] w]
    it "1" $ checkEval p ["International Business Machines"]                  "IBM"
    it "2" $ checkEval p ["Principles Of Programming Languages"]              "POPL"
    it "3" $ checkEval p ["International Conference on Software Engineering"] "ICSE"

  describe "example #5 from the paper" $ do
    let pos1 = pos [LeftParenTok] [NumTok, SlashTok]
    let pos2 = pos [SlashTok, NumTok] [RightParenTok]
    let p = loop 0 $ \w -> [substr (pos1 w) (pos2 w), ConstStr " # "]
    it "1" $ checkEval p ["(6/7)(4/5)(14/1)"] "6/7 # 4/5 # 14/1 # "
    it "2" $ checkEval p ["49(28/11)(14/1)"]  "28/11 # 14/1 # "
    it "3" $ checkEval p ["() (28/11)(14/1)"] "28/11 # 14/1 # "

  describe "example #6 from the paper" $ do
    let pos1 = pos eps [NonSpaceTok]
    let pos2 = pos [NonSpaceTok] [SpaceTok, NonSpaceTok]
    let p = loop 0 $ \w ->
            [ substr (pos1 w) (pos2 w)
            , ConstStr " "
            , substr2 [NonSpaceTok] (-1 :: Int)
            ]
    it "1" $ checkEval p ["    Oege   de     Moor  "]         "Oege de Moor"
    it "2" $ checkEval p ["Kathleen   Fisher    AT&T Labs "]  "Kathleen Fisher AT&T Labs"
