{-# LANGUAGE OverloadedStrings #-}
module T10
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
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, testSpec)
import TestUtility

spec :: Spec
spec = do
  describe "simple echo expressions" $ do
    evaluatesTo
      "echo(\"Hello\")"
      (HiValueAction (HiActionEcho "Hello"))
    evaluatesTo "echo(\"Hello\")!" HiValueNull
  describe "lazy evaluation" $ do
    evaluatesTo "if(2 == 2, 1, 2(2))" (makeVNum 1)
    evaluatesTo "true || null" (HiValueBool True)
    throwsError "false || 1()" HiErrorInvalidFunction
    evaluatesTo "false && null" (HiValueBool False)
    throwsError "true && 1()" HiErrorInvalidFunction
  describe "need write permission (echo)" $ do
    throwsIOError "echo(\"Give me permissions!\")!" [AllowRead, AllowTime]

unitTests :: IO TestTree
unitTests = testSpec "Task 10" spec
