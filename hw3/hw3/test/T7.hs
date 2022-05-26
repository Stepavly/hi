module T7
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
    evaluatesTo "mkdir(\"temp_7_1\")!" HiValueNull
    evaluatesTo "cd(\"temp_7_1\")!" HiValueNull
    evaluatesTo "write(\"a\", \"Works\")!" HiValueNull
    evaluatesTo "write(\"b\", \" fine\")!" HiValueNull
    evaluatesTo "read(\"a\")! + read(\"b\")!" (makeVString "Works fine")
  describe "need read permission (read)" $ do
    evaluatesTo "mkdir(\"temp_7_2\")!" HiValueNull
    evaluatesTo "cd(\"temp_7_2\")!" HiValueNull
    evaluatesTo "write(\"a\", \"Next read will throwIO\")!" HiValueNull
    throwsIOError "read(\"a\")!" [AllowWrite, AllowTime]
  describe "need read permission (cd)" $ do
    evaluatesTo "mkdir(\"temp_7_3\")!" HiValueNull
    throwsIOError "cd(\"temp_7_3\")!" [AllowWrite, AllowTime]
  describe "need read permission (cwd)" $ do
    throwsIOError "cwd!" [AllowWrite, AllowTime]
  describe "need write permission (mkdir)" $ do
    throwsIOError "mkdir(\"temp_7_4\")!" [AllowRead, AllowTime]

unitTests :: IO TestTree
unitTests = testSpec "Task 7" spec
