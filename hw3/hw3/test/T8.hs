module T8
  (
    unitTests
  ) where
import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Base (Semigroup (stimes))
import HW3.Action (HiPermission (..))
import HW3.Base (HiError (..), HiFun (..), HiValue (..))
import HW3.Evaluator (eval)
import qualified HW3.Evaluator
import Hedgehog (forAll, property, (===))
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, testSpec)
import TestUtility

spec :: Spec
spec = do
  describe "all permissions test" $ do
    printsTo
      "parse-time(\"2021-12-15 00:00:00 UTC\") + 1000"
      "parse-time(\"2021-12-15 00:16:40 UTC\")"
    evaluatesTo
      "parse-time(\"2021-12-15 00:37:51.000890793 UTC\") - parse-time(\"2021-12-15 00:37:47.649047038 UTC\")"
      (makeVNum 3.351843755)
    printsTo
      "parse-time(\"2021-01-01 00:00:00 UTC\") + 365 * 24 * 60 * 60"
      "parse-time(\"2022-01-01 00:00:00 UTC\")"
  describe "need time permission (now)" $ do
    throwsIOError "now!" [AllowRead, AllowWrite]

unitTests :: IO TestTree
unitTests = testSpec "Task 8" spec
