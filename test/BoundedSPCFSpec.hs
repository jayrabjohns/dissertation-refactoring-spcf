module BoundedSPCFSpec where

import qualified Data.Map as Map
import BoundedSPCFSpec
import Test.HUnit

tests :: Test
tests = TestList 
    [ reduceExpression
    
    ]

reduceExpression :: Test
reduceExpression =
    TestLabel 
        "should reduce an expression"
        (
TestCase
    ( do
        -- 12 + 4 + 2 = 18
        let expression = Literal 12 `Plus` Apply (Abstract "x" (Variable "x")) (Literal 4 `Plus` Literal 2)
        let result = runEval Map.empty (eval expression)
        case result of
          Right val -> assertEqual "should equal 18" (Nat 18) val
          Left err -> assertFailure err
    )
        )