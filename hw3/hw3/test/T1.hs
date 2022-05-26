module T1
  (
    unitTests
  ) where
import HW3.Base (HiError (..), HiValue (..))
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, testSpec)
import TestUtility (evaluatesTo, throwsError)

spec :: Spec
spec = do
  describe "simple expressions" $ do
    evaluatesTo "add(1, 2)" (HiValueNumber 3)
    evaluatesTo " div ( 3 , 4 )" (HiValueNumber (3 / 4))
    throwsError "div   (mul(1, 0), sub( 0,0 ))" HiErrorDivideByZero
  describe "complex expressions" $ do
    evaluatesTo "  mul(  (( 3) ) ,( add) (4,div(4 ,3)  ) )  " (HiValueNumber 16)
    evaluatesTo "((((10))))" (HiValueNumber 10)
  describe "eval errors" $ do
    throwsError "mul(0, (div(1, 0)))" HiErrorDivideByZero
    throwsError "mul(add)" HiErrorArityMismatch
    throwsError "sub(add, 2, 3)" HiErrorArityMismatch
    throwsError "(2)(3, 4)" HiErrorInvalidFunction
    throwsError "add(sub, 1)" HiErrorInvalidArgument

unitTests :: IO TestTree
unitTests = testSpec "Task 1" spec
