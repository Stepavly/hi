module TestUtility
  (
    evaluatesTo
  , parsedTo
  , printsTo
  , throwsError
  , throwsIOError
  , genNum
  , genBool
  , genFun
  , genString
  , genByteString
  , genNull
  , genList
  , genRandom
  , genValue
  , genExpr
  , makeFun
  , makeExpr
  , makeNum
  , makeVNum
  , makeString
  , makeVString
  , makeVList
  , makeVBytes
  , makeVMap
  ) where

import HW3.Action (HiPermission (..), HIO (runHIO), PermissionException)
import HW3.Base
import HW3.Parser
import HW3.Evaluator

import Data.Set (Set, fromList)
import Test.Tasty.Hspec (Expectation, shouldBe, shouldNotBe, shouldThrow, anyException, shouldSatisfy, shouldNotSatisfy, it, SpecWith)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Range (Range(Range), linear)
import Data.Ratio ((%))
import qualified Hedgehog.Internal.Range as Range
import qualified Data.Text
import qualified Data.Sequence
import qualified Data.Sequence as Sequence
import Data.Word (Word8)
import qualified Data.ByteString
import HW3.Pretty (prettyValue)
import Control.Exception (try, throwIO)
import Data.Either (isLeft)
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Prettyprinter (Doc, pretty)
import Prettyprinter.Render.Terminal (AnsiStyle)
import qualified Data.Map

fullPermissions :: Set HiPermission
fullPermissions = Data.Set.fromList [AllowRead, AllowWrite, AllowTime]

tryParse :: String -> IO (Maybe HiExpr)
tryParse expr =
  case parse expr of
    Left _ -> return Nothing
    Right e -> return $ Just e

evaluateWithFullPerms :: String -> IO (Maybe (Either HiError HiValue))
evaluateWithFullPerms expr = do
  e <- tryParse expr
  case e of
    Nothing -> return Nothing
    Just e' -> do
      res <- runHIO (eval e') fullPermissions
      return $ Just res

evaluatesTo :: String -> HiValue -> SpecWith ()
evaluatesTo expr expected = it expr $ do
  real <- evaluateWithFullPerms expr
  real `shouldBe` Just (Right expected)

parsedTo :: String -> HiExpr -> SpecWith ()
parsedTo expr expected = it expr $ do
  real <- tryParse expr
  real `shouldBe` Just expected

printsTo :: String -> String -> SpecWith ()
printsTo expr expected = it expr $ do
  expr' <- evaluateWithFullPerms expr
  case expr' of
    Nothing -> expr' `shouldNotBe` Nothing
    Just expr'' -> case expr'' of
      Left _ -> expr'' `shouldNotSatisfy` isLeft
      Right e -> show (prettyValue e) `shouldBe` show (pretty expected)

exprEvaluatesTo :: HiExpr -> HiValue -> Expectation
exprEvaluatesTo expr value = do
  real <- runHIO (eval expr) fullPermissions
  real `shouldBe` Right value

throwsError :: String -> HiError -> SpecWith ()
throwsError expr err = it expr $ do
  real <- evaluateWithFullPerms expr
  real `shouldBe` Just (Left err)

throwsIOError :: String -> [HiPermission] -> SpecWith ()
throwsIOError expr perms = it expr $ do
  e <- tryParse expr
  case e of
    Nothing -> e `shouldNotBe` Nothing
    Just e' -> shouldThrow (runHIO (eval e') (fromList perms)) anyException

numRange :: Range Integer
numRange = linear (-100 :: Integer) (100 :: Integer)

denRange :: Range Integer
denRange = linear 1 100

lengthRange :: Range Int
lengthRange = linear 1 10

argsLengthRange :: Range Int
argsLengthRange = linear 0 3

genNum :: Gen HiValue
genNum = do
  num <- Gen.integral numRange
  den <- Gen.integral denRange
  return $ HiValueNumber (num % den)

genBool :: Gen HiValue
genBool = HiValueBool <$> Gen.bool

genFun :: Gen HiValue
genFun = HiValueFunction <$> Gen.element
  [ HiFunDiv
  , HiFunMul
  , HiFunAdd
  , HiFunSub
  , HiFunNot
  , HiFunAnd
  , HiFunOr
  , HiFunLessThan
  , HiFunGreaterThan
  , HiFunEquals
  , HiFunNotLessThan
  , HiFunNotGreaterThan
  , HiFunNotEquals
  , HiFunIf
  , HiFunLength
  , HiFunToUpper
  , HiFunToLower
  , HiFunReverse
  , HiFunTrim
  , HiFunList
  , HiFunRange
  , HiFunFold
  , HiFunPackBytes
  , HiFunUnpackBytes
  , HiFunEncodeUtf8
  , HiFunDecodeUtf8
  , HiFunZip
  , HiFunUnzip
  , HiFunSerialise
  , HiFunDeserialise ]

genString :: Gen HiValue
genString = do
  str <- Gen.string lengthRange Gen.enumBounded
  return $ HiValueString $ Data.Text.pack str

genByteString :: Gen HiValue
genByteString = do
  bytes <- Gen.bytes lengthRange
  return $ HiValueBytes bytes

genNull :: Gen HiValue
genNull = return HiValueNull

genList :: Gen HiValue
genList = do
  list <- Gen.list lengthRange genValue
  return $ HiValueList (Sequence.fromList list)

genRandom :: Gen (Int, Int, HiAction)
genRandom = do
  l <- Gen.integral numRange
  r <- Gen.integral (linear l 100)
  let lInt = fromInteger l :: Int
      rInt = fromInteger r :: Int
  return (lInt, rInt, HiActionRand lInt rInt)

genValue :: Gen HiValue
genValue = Gen.choice [genNum, genBool, genFun, genString, genByteString, genNull]

genExpr :: Gen HiExpr
genExpr = Gen.frequency [(4, HiExprValue <$> genValue), (1, do
  f <- genExpr
  args <- Gen.list argsLengthRange genExpr
  return $ HiExprApply f args)]

makeFun :: HiFun -> [HiExpr] -> HiExpr
makeFun f = HiExprApply (HiExprValue $ HiValueFunction f)

makeExpr :: HiValue -> HiExpr
makeExpr = HiExprValue

makeVNum :: Rational -> HiValue
makeVNum = HiValueNumber

makeNum :: Rational -> HiExpr
makeNum = makeExpr . makeVNum

makeVString :: String -> HiValue
makeVString s = HiValueString $ Data.Text.pack s

makeString :: String -> HiExpr
makeString = makeExpr . makeVString

makeVList :: [HiValue] -> HiValue
makeVList list = HiValueList $ Sequence.fromList list

makeVBytes :: [Word8] -> HiValue
makeVBytes list = HiValueBytes $ Data.ByteString.pack list

makeVMap :: [(HiValue, HiValue)] -> HiValue
makeVMap list = HiValueDict $ Data.Map.fromList list
