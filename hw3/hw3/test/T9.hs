module T9
  (
    propertyTests
  ) where
import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Base (Semigroup (stimes))
import HW3.Action (HiPermission (..))
import HW3.Base
import HW3.Evaluator (eval)
import qualified HW3.Evaluator
import Hedgehog (forAll, property, (===), PropertyT)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, testSpec)
import TestUtility

repeatRand :: HiAction -> Int -> Int -> Int -> PropertyT IO ()
repeatRand _ _ _ 0 = return ()
repeatRand rand l r iter = do
  x <- liftIO $ runAction rand
  (makeVNum (fromIntegral l) <= x && x <= makeVNum (fromIntegral r)) === True
  repeatRand rand l r (iter - 1)

propertyTests :: IO TestTree
propertyTests = return $ testGroup "Task 9 properties"
  [testProperty "random in range" $ property $ do
    temp <- forAll genRandom
    let (l, r, rand) = temp
    repeatRand rand l r 1000]
    