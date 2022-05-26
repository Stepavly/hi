{-# LANGUAGE OverloadedStrings #-}
module T4
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
    evaluatesTo "length(\"Nice test\")" (HiValueNumber 9)
    evaluatesTo "to-upper(\"to-lower\")" (HiValueString "TO-LOWER")
    evaluatesTo "to-lower(\"TO-UPPER\")" (HiValueString "to-upper")
    evaluatesTo "to-lower(reverse(\"Murder for a jar of red rum\"))"
      (HiValueString "mur der fo raj a rof redrum")
    evaluatesTo "length(trim(\" f p  \"))" (HiValueNumber 3)
    evaluatesTo "\"6\" / \"3\" + \"=2\" " (HiValueString "6/3=2")
  describe "indexing" $ do
    evaluatesTo "\"Hello World\"(0)" (HiValueString "H")
    evaluatesTo "\"Hello World\"(7)" (HiValueString "o")
    evaluatesTo "\"Hello World\"(-1)" HiValueNull
    evaluatesTo "\"Hello World\"(99)" HiValueNull
    throwsError "\"Hello World\"(1/2)" HiErrorInvalidArgument
    throwsError "\"Hello World\"(true)" HiErrorInvalidArgument
  describe "slicing" $ do
    evaluatesTo "\"Hello World\"(0, 5)" (HiValueString "Hello")
    evaluatesTo "\"Hello World\"(2, 4)" (HiValueString "ll")
    evaluatesTo "\"Hello World\"(0, -4)" (HiValueString "Hello W")
    evaluatesTo "\"Hello World\"(-4, -1)" (HiValueString "orl")
    evaluatesTo "\"Hello World\"(2, null)" (HiValueString "llo World")
    evaluatesTo "\"Hello World\"(null, 5)" (HiValueString "Hello")
    evaluatesTo "\"Hello World\"(null, null)" (HiValueString "Hello World")
    evaluatesTo "\"Hello World\"(null, 1000)" (HiValueString "Hello World")
    evaluatesTo "\"Hello World\"(-1000, null)" (HiValueString "Hello World")
    evaluatesTo "\"Hello World\"(-1000, 1000)" (HiValueString "Hello World")
    evaluatesTo "\"Hello World\"(3, 2)" (HiValueString "")
    throwsError "\"Hello World\"(true, 2)" HiErrorInvalidArgument
    throwsError "\"Hello World\"(null, 0.5)" HiErrorInvalidArgument

unitTests :: IO TestTree
unitTests = testSpec "Task 4" spec
