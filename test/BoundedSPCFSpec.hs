module BoundedSPCFSpec where

import BoundedSPCF
import Test.HUnit

tests :: Test
tests =
  TestList
    [ evalCase,
      evalCase0
    ]

evalCase :: Test
evalCase = do
  let f = Lambda "p" (Base `Cross` Base `Cross` Base) $ Lambda "i" Base $ Case (Variable "i") (Variable "p")
  let p = Product [Numeral 1, Numeral 2, Numeral 3]
  let i = Numeral 3
  let term = Apply (Apply f p) i
  let expectedResult = Nat 3
  TestLabel
    "should evaluate to nth term of p for (Case n p)"
    $ assertEval term emptyEnv expectedResult

evalCase0 :: Test
evalCase0 = do
  let f = Lambda "p" (Base `Cross` Base `Cross` Base) $ Lambda "i" Base $ Case (Variable "i") (Variable "p")
  let p = Product [Numeral 1, Numeral 2, Numeral 3]
  let i = Numeral 0
  let term = Apply (Apply f p) i
  let expectedResult = Closure emptyEnv emptyProduct
  TestLabel
    "should evaluate to the emtpy product for (Case 0 p)"
    $ assertEval term emptyEnv expectedResult

assertEval :: Term -> Environment -> Value -> Test
assertEval term env expectedVal =
  TestCase
    ( do
        result <- runEvalIO (eval term) env
        assertEqual
          ( "Term doesn't evaluate to the expected result. Term: "
              ++ show term
              ++ ". Environment: "
              ++ show env
          )
          expectedVal
          result
    )