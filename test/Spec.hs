import Test.Hspec
import Eval qualified
import Examples qualified

main :: IO ()
main = hspec $ do
  Eval.spec
  Examples.spec
