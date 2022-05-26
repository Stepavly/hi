import T1
import T2
import T3
import T4
import T5
import T6
import T7
import T8
import T9
import T10
import T11
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  unit1 <- T1.unitTests
  unit2 <- T2.unitTests
  prop2 <- T2.propertyTests
  unit3 <- T3.unitTests 
  unit4 <- T4.unitTests 
  unit5 <- T5.unitTests
  unit6 <- T6.unitTests 
  prop6 <- T6.propertyTests 
  unit7 <- T7.unitTests 
  unit8 <- T8.unitTests 
  prop9 <- T9.propertyTests
  unit10 <- T10.unitTests 
  unit11 <- T11.unitTests
  defaultMain $ testGroup "Tests" 
    [unit1, 
     unit2, prop2, 
     unit3, 
     unit4,
     unit5,
     unit6, prop6,
     unit7,
     unit8,
     prop9,
     unit10,
     unit11]