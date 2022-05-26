{-# LANGUAGE OverloadedStrings #-}
module T5
  (
    unitTests
  ) where
import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Base (Semigroup (stimes))
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
    evaluatesTo "list(1, 2, 3)" (makeVList $ map makeVNum [1..3])
    evaluatesTo "range(5, 10.3)" (makeVList $ map makeVNum [5..10])
    evaluatesTo "fold(add, [11, 22, 33])" (makeVNum 66)
    evaluatesTo "fold(mul, [11, 22, 33])" (makeVNum 7986)
    evaluatesTo "fold(div, [11, 22, 33])" (makeVNum (1 / 66))
    evaluatesTo "length([1, true, \"Hello\"])" (makeVNum 3)
    evaluatesTo "reverse([1, true, \"Hello\"])"
      (makeVList [makeVString "Hello", HiValueBool True, makeVNum 1])
    evaluatesTo "[1, 2] + [3, 4, 5]" (makeVList $ map makeVNum [1..5])
    evaluatesTo "[0, \"x\"] * 3" (makeVList
      [makeVNum 0, makeVString "x",
       makeVNum 0, makeVString "x",
       makeVNum 0, makeVString "x"])
  describe "complex expressions" $ do
    evaluatesTo "fold(add, [2, 5] * 3)" (makeVNum 21)
    evaluatesTo "[fold(div, [\"path\", \"to\", \"file\"])] * 3"
      (makeVList (stimes 3 [makeVString "path/to/file"]))
    evaluatesTo "reverse(range(0.5, 70/8))" (makeVList $ map makeVNum [8.5, 7.5..0.5])
  describe "indexing" $ do
    evaluatesTo "([2, 5] * 3)(0)" (makeVNum 2)
    evaluatesTo "([2, 5] * 3)(1)" (makeVNum 5)
    evaluatesTo "reverse(range(0.5, 70/8))(3)" (makeVNum 5.5)
  describe "slicing" $ do
    evaluatesTo "fold(add, ([2, 5] * 3)(1, 4))" (makeVNum 12) -- 2 |5+2+5| 2 5
    evaluatesTo "[1, 2, 3, 4, 5](-3, 3)" (makeVList [makeVNum 3])
    evaluatesTo "[1, 2, 3, 4, 5](-6, null)" (makeVList $ map makeVNum [1..5])
    evaluatesTo "[1, 2, 3, 4, 5](4, 3)" (makeVList [])

unitTests :: IO TestTree
unitTests = testSpec "Task 5" spec
