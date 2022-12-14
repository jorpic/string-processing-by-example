module Examples (spec) where

import Test.Hspec
import StringExpr
import StringExpr.Builder
import Utils

spec :: Spec
spec = do
  describe "example #2 from the paper" $ do
    let p = substr (pos eps [NumTok] (0 :: Int)) (-1 :: Int)
    it "1" $ checkAtom p ["BTR KRNL WK CORN 15Z"]      "15Z"
    it "2" $ checkAtom p ["CAMP DRY DBL NDL 3.6 OZ"]   "3.6 OZ"
    it "3" $ checkAtom p ["CHORE BOY HD SC SPNG 1 PK"] "1 PK"
    it "4" $ checkAtom p ["FRENCH WORCESTERSHIRE 5 Z"] "5 Z"
    it "5" $ checkAtom p ["O F TOMATO PASTE 6 OZ"]     "6 OZ"

  describe "example #3 from the paper" $ do
    let p = substr (0 :: Int) (pos [SlashTok] eps (-1 :: Int))
    it "1" $ checkAtom p ["Company/Code/index.html"]     "Company/Code/"
    it "2" $ checkAtom p ["Company/Docs/Spec/specs.doc"] "Company/Docs/Spec/"

  describe "example #4 from the paper" $ do
    let p = loop 0 $ \w -> [substr2 [UpperTok] w]
    it "1" $ checkAtom p ["International Business Machines"]                  "IBM"
    it "2" $ checkAtom p ["Principles Of Programming Languages"]              "POPL"
    it "3" $ checkAtom p ["International Conference on Software Engineering"] "ICSE"

  describe "example #5 from the paper" $ do
    let pos1 = pos [LeftParenTok] [NumTok, SlashTok]
    let pos2 = pos [SlashTok, NumTok] [RightParenTok]
    let p = loop 0 $ \w -> [substr (pos1 w) (pos2 w), ConstStr " # "]
    it "1" $ checkAtom p ["(6/7)(4/5)(14/1)"] "6/7 # 4/5 # 14/1 # "
    it "2" $ checkAtom p ["49(28/11)(14/1)"]  "28/11 # 14/1 # "
    it "3" $ checkAtom p ["() (28/11)(14/1)"] "28/11 # 14/1 # "

  describe "example #6 from the paper" $ do
    let pos1 = pos eps [NonSpaceTok]
    let pos2 = pos [NonSpaceTok] [SpaceTok, NonSpaceTok]
    let p = loop 0 $ \w ->
            [ substr (pos1 w) (pos2 w)
            , ConstStr " "
            , substr2 [NonSpaceTok] (-1 :: Int)
            ]
    it "1" $ checkAtom p
      ["    Oege   de     Moor  "]
      "Oege de Moor"
    it "2" $ checkAtom p
      ["Kathleen   Fisher    AT&T Labs "]
      "Kathleen Fisher AT&T Labs"

  describe "example #7 from the paper" $ do
    let p = Switch
            [ ( [[match 0 [CharTok], match 1 [CharTok]]]
              , Concat [inp 1, ConstStr "(", inp 2, ConstStr ")"]
              )
            , ( [[noMatch 0 [CharTok]], [noMatch 1 [CharTok]]]
              , Concat []
              )
            ]
    it "1" $ checkExpr p ["Alex", "Asst."  ] "Alex(Asst.)"
    it "2" $ checkExpr p ["Jim",  "Manager"] "Jim(Manager)"
    it "3" $ checkExpr p ["Ryan", ""       ] ""
    it "4" $ checkExpr p ["",     "Asst."  ] ""

  describe "example #8 from the paper" $ do
    let p = Switch
          [ ( [[match 0 [SlashTok]]]
            , Concat [substr (pos [StartTok] eps (0::Int)) (pos eps [SlashTok] (0::Int))]
            )
          , ( [[match 0 [DotTok]]]
            , Concat [substr (pos [DotTok] eps (0::Int)) (pos eps [DotTok] (1::Int))]
            )
          , ( [[match 0 [HyphenTok]]]
            , Concat [substr (pos [HyphenTok] eps (1::Int)) (pos [EndTok] eps (0::Int))]
            )

          ]
    it "1" $ checkExpr p ["01/21/2001"] "01"
    it "2" $ checkExpr p ["22.02.2002"] "02"
    it "3" $ checkExpr p ["2003-23-03"] "03"
