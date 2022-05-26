module T3
  (
    unitTests
  ) where
import Control.Monad.IO.Class (MonadIO (liftIO))
import HW3.Base (HiError (..), HiFun (..), HiValue (..))
import HW3.Evaluator (eval)
import qualified HW3.Evaluator
import Hedgehog (forAll, property, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, testSpec)
import TestUtility

spec :: Spec
spec = do
  describe "simple expressions" $ do
    parsedTo "1 - 2 - 3"
      (makeFun HiFunSub [makeFun HiFunSub [makeNum 1, makeNum 2], makeNum 3])
    parsedTo "1 - (2 - 3)"
      (makeFun HiFunSub [makeNum 1, makeFun HiFunSub [makeNum 2, makeNum 3]])
    parsedTo "1 * 2 + 3"
      (makeFun HiFunAdd [makeFun HiFunMul [makeNum 1, makeNum 2], makeNum 3])
    parsedTo "1 / (2 + 3)"
      (makeFun HiFunDiv [makeNum 1, makeFun HiFunAdd [makeNum 2, makeNum 3]])
    parsedTo "1 /= 2 / 3"
      (makeFun HiFunNotEquals [makeNum 1, makeFun HiFunDiv [makeNum 2, makeNum 3]])
  describe "complex expressions" $ do
    evaluatesTo "1 - 2 - 3" (HiValueNumber (-4))
    evaluatesTo "11 / 22 * 7" (HiValueNumber (7 / 2))
    evaluatesTo "10 == 2*5 && 143 == 11*13" (HiValueBool True)
    evaluatesTo "(1 / 2 < 1 / 3) /= true" (HiValueBool True)

unitTests :: IO TestTree
unitTests = testSpec "Task 3" spec
