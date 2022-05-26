module T6
  (
    unitTests
  , propertyTests
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
    evaluatesTo "pack-bytes([ 3, 255, 158, 32 ])" (makeVBytes [0x03, 0xff, 0x9e, 0x20])
    evaluatesTo "unpack-bytes([#10   20 30  #])" (makeVList $ map makeVNum [16, 32, 48])
    evaluatesTo "encode-utf8(\"Hello!\")" (makeVBytes [0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x21])
    evaluatesTo "decode-utf8([# 48 65 6c 6c 6f #])" (makeVString "Hello")
    evaluatesTo "decode-utf8([# c3 28 #])" HiValueNull
    evaluatesTo "[# 00 ff #] + [# 01 e3 #]" (makeVBytes [0x00, 0xff, 0x01, 0xe3])
    evaluatesTo "[# 00 ff #] * 3" (makeVBytes $ stimes 3 [0x00, 0xff])
  describe "complex expressions" $ do
    evaluatesTo "pack-bytes(range(30, 40))" (makeVBytes [30..40])
  describe "indexing" $ do
    evaluatesTo "([# 00 ff #] * 3 + [# 01 e3 #])(0)" (makeVNum 0x00)
    evaluatesTo "([# 00 ff #] * 3 + [# 01 e3 #])(1)" (makeVNum 0xff)
    evaluatesTo "([# 00 ff #] * 3 + [# 01 e3 #])(6)" (makeVNum 0x01)
    evaluatesTo "([# 00 ff #] * 3 + [# 01 e3 #])(7)" (makeVNum 0xe3)
    evaluatesTo "([# 00 ff #] * 3 + [# 01 e3 #])(-1)" HiValueNull
    evaluatesTo "([# 00 ff #] * 3 + [# 01 e3 #])(8)" HiValueNull
    throwsError "([# 00 ff #] * 3 + [# 01 e3 #])(8.1)" HiErrorInvalidArgument
  describe "slicing" $ do
    evaluatesTo "([# 00 ff #] * 3 + [# 01 e3 #])(null, 3)" (makeVBytes [0x00, 0xff, 0x00])
    evaluatesTo "([# 00 ff #] * 3 + [# 01 e3 #])(-3, null)" (makeVBytes [0xff, 0x01, 0xe3])
    evaluatesTo "length(([# 00 ff #] * 3 + [# 01 e3 #])(null, null))" (makeVNum 8)
    evaluatesTo "([# 00 ff #] * 3 + [# 01 e3 #])(-1, 1)" (makeVBytes [])
    evaluatesTo "length(([# 00 ff #] * 3 + [# 01 e3 #])(-1, 1))" (makeVNum 0)

propertyTests :: IO TestTree
propertyTests = return $ testGroup "Task 6 properties"
  [testProperty "unzip(zip(A)) ≡ A" $ property $ do
    a <- forAll genByteString
    res <- liftIO $ eval (makeFun HiFunUnzip [makeFun HiFunZip [makeExpr a]])
    res === Right a,
   testProperty "deserialise(serialise(A)) ≡ A" $ property $ do
    a <- forAll genValue
    res <- liftIO $ eval (makeFun HiFunDeserialise [makeFun HiFunSerialise [makeExpr a]])
    res === Right a,
   testProperty "decode-utf8(encode-utf8(A)) ≡ A" $ property $ do
    a <- forAll genString
    res <- liftIO $ eval (makeFun HiFunDecodeUtf8 [makeFun HiFunEncodeUtf8 [makeExpr a]])
    res === Right a
    ]

unitTests :: IO TestTree
unitTests = testSpec "Task 6" spec
