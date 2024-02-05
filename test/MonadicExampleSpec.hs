module MonadicExampleSpec where

import qualified Data.Map as Map
import MonadicExample (Expression (..), Value (..), eval)
import Test.HUnit

tests :: Test
tests = TestList [TestLabel "should reduce an expression" reduceExpression]

reduceExpression :: Test
reduceExpression =
  TestCase
    ( do
        -- 12 + 4 + 2 = 18
        let expression = Literal 12 `Plus` Apply (Abstract "x" (Variable "x")) (Literal 4 `Plus` Literal 2)
        let result = eval Map.empty expression
        assertEqual "should equal 18" (Nat 18) result
    )