{-# LANGUAGE OverloadedStrings #-}
module T11
  (
    unitTests
  ) where
import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Base (Semigroup (stimes))
import HW3.Action (HiPermission (..))
import HW3.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiValue (..))
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
    evaluatesTo
      "{ \"width\": 120, \"height\": 80 }"
      (makeVMap [(makeVString "width", makeVNum 120), (makeVString "height", makeVNum 80)])
    evaluatesTo "{ \"width\": 120, \"height\": 80 }(\"width\")" (makeVNum 120)
    evaluatesTo "{ \"width\": 120, \"height\": 80 }(\"height\")" (makeVNum 80)
  describe "map keys" $ do
    evaluatesTo
      "keys({ \"width\": 120, \"height\": 80 })"
      (makeVList $ map makeVString ["height", "width"])
    evaluatesTo "keys(count(range(1, 5)))" (makeVList $ map makeVNum [1..5])
  describe "map values" $ do
    evaluatesTo
      "values({ \"width\": 120, \"height\": 80 })"
      (makeVList $ map makeVNum [80, 120])
    evaluatesTo "fold(add, fold(add, values(invert(count(\"world\")))))" (makeVString "wrold")
  describe "map count" $ do
    evaluatesTo "count(\"XXXOX\")"
      (makeVMap [(makeVString "O", makeVNum 1), (makeVString "X", makeVNum 4)])
    evaluatesTo "count([true, true, false, true])"
      (makeVMap [(HiValueBool False, makeVNum 1), (HiValueBool True, makeVNum 3)])
  describe "map invert" $ do
    evaluatesTo "invert(count(\"big blue bag\"))"
      (makeVMap
        [(makeVNum 1, makeVList $ map makeVString ["u", "l", "i", "e", "a"]),
         (makeVNum 2, makeVList $ map makeVString ["g", " "]),
         (makeVNum 3, makeVList [makeVString "b"])])

unitTests :: IO TestTree
unitTests = testSpec "Task 11" spec
