module MonadicExampleSpec where

import qualified Data.Map as Map
import MonadicExample
import Test.HUnit

tests :: Test
tests = TestList [TestLabel "should reduce an expression" reduceExpression]

reduceExpression :: Test
reduceExpression =
  TestCase
    ( do
        -- 12 + 4 + 2 = 18
        let expression = Literal 12 `Plus` Apply (Abstract "x" (Variable "x")) (Literal 4 `Plus` Literal 2)
        let result = runEval (eval Map.empty expression)
        case result of
          Right val -> assertEqual "should equal 18" (Nat 18) val
          Left err -> assertFailure err
    )