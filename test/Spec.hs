import  Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "test" $ it "fails" $ True `shouldBe` False
