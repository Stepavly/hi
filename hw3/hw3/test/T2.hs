module T2
  (
    unitTests
  , propertyTests
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
  describe "simple boolean algebra" $ do
    evaluatesTo "true" (HiValueBool True)
    evaluatesTo "false" (HiValueBool False)
    evaluatesTo "not(true)" (HiValueBool False)
    evaluatesTo "not(false)" (HiValueBool True)
    evaluatesTo "and(false, false)" (HiValueBool False)
    evaluatesTo "and(false, true)" (HiValueBool False)
    evaluatesTo "and(true, false)" (HiValueBool False)
    evaluatesTo "and(true, true)" (HiValueBool True)
    evaluatesTo "or(false, false)" (HiValueBool False)
    evaluatesTo "or(false, true)" (HiValueBool True)
    evaluatesTo "or(true, false)" (HiValueBool True)
    evaluatesTo "or(true, true)" (HiValueBool True)
  describe "equality checking" $ do
    parsedTo "not-equals(true, 2)"
      (makeFun HiFunNotEquals [makeExpr (HiValueBool True), makeNum 2])
    parsedTo "not-less-than(1, 2)"
      (makeFun HiFunNotLessThan [makeNum 1, makeNum 2])
    parsedTo "not-greater-than(1, false)"
      (makeFun HiFunNotGreaterThan [makeNum 1, makeExpr (HiValueBool False)])
    evaluatesTo "equals(1, 2)" (HiValueBool False)
    evaluatesTo "equals(null, null)" (HiValueBool True)
    evaluatesTo "equals(1, true)" (HiValueBool False)
    evaluatesTo "\"passed\" / \"test\" == \"passed/test\"" (HiValueBool True)
  describe "basic compares" $ do
    evaluatesTo "less-than(1, true)" (HiValueBool False)
    evaluatesTo "less-than(true, 1)" (HiValueBool True)
    evaluatesTo "less-than(false, true)" (HiValueBool True)

propertyTests :: IO TestTree
propertyTests = return $ testGroup "Task 2 properties"
  [testProperty "less-than transitivity" $ property $ do -- a < b && b < c => a < b === ! (a < b && b < c) || (a < c)
    a <- forAll genValue
    b <- forAll genValue
    c <- forAll genValue
    res <- liftIO $ eval
      (
        makeFun HiFunOr -- ! (a < b && b < c) || (a < c)
          [makeFun HiFunNot -- ! (a < b && b < c)
            [makeFun HiFunAnd -- a < b && b < c
              [makeFun HiFunLessThan [makeExpr a, makeExpr b], -- a < b
               makeFun HiFunLessThan [makeExpr b, makeExpr c]]], -- b < c
           makeFun HiFunLessThan [makeExpr a, makeExpr c]] -- a < c
      )
    res === Right (HiValueBool True),
   testProperty "greater-than(A, B) ≡ less-than(B, A)" $ property $ do
    a <- forAll genValue
    b <- forAll genValue
    res <- liftIO $ eval
      (makeFun HiFunEquals [makeFun HiFunGreaterThan [makeExpr a, makeExpr b],
                            makeFun HiFunLessThan [makeExpr b, makeExpr a]])
    res === Right (HiValueBool True),
   testProperty "not-equals(A, B) ≡ not(equals(A, B))" $ property $ do
    a <- forAll genValue
    b <- forAll genValue
    res <- liftIO $ eval
      (makeFun HiFunEquals [makeFun HiFunNotEquals [makeExpr a, makeExpr b],
                            makeFun HiFunNot [makeFun HiFunEquals [makeExpr a, makeExpr b]]])
    res === Right (HiValueBool True),
   testProperty "not-less-than(A, B) ≡ not(less-than(A, B))" $ property $ do
    a <- forAll genValue
    b <- forAll genValue
    res <- liftIO $ eval
      (makeFun HiFunEquals [makeFun HiFunNotLessThan [makeExpr a, makeExpr b],
                            makeFun HiFunNot [makeFun HiFunLessThan [makeExpr a, makeExpr b]]])
    res === Right (HiValueBool True),
   testProperty "not-greater-than(A, B) ≡ not(greater-than(A, B))" $ property $ do
    a <- forAll genValue
    b <- forAll genValue
    res <- liftIO $ eval
      (makeFun HiFunEquals [makeFun HiFunNotGreaterThan [makeExpr a, makeExpr b],
                            makeFun HiFunNot [makeFun HiFunGreaterThan [makeExpr a, makeExpr b]]])
    res === Right (HiValueBool True),
   testProperty "if(true, A, B) ≡ A" $ property $ do
    a <- forAll genExpr
    b <- forAll genExpr
    a' <- liftIO $ eval a
    res <- liftIO $ eval (makeFun HiFunIf [makeExpr (HiValueBool True), a, b])
    res === a',
   testProperty "if(false, A, B) ≡ B" $ property $ do
    a <- forAll genExpr
    b <- forAll genExpr
    b' <- liftIO $ eval b
    res <- liftIO $ eval (makeFun HiFunIf [makeExpr (HiValueBool False), a, b])
    res === b'
    ]

unitTests :: IO TestTree
unitTests = testSpec "Task 2" spec
