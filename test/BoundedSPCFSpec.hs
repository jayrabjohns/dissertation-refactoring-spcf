module BoundedSPCFSpec where

import BoundedSPCF
import Test.HUnit

tests :: Test
tests =
  TestList
    [ evalCase,
      evalCase0,
      evalCaseWithCatchAsNumeral
    ]

evalCase :: Test
evalCase = do
  let f = Lambda "p" (Base `Cross` Base `Cross` Base) $ Lambda "i" Base $ Case (Variable "i") (Variable "p")
  let p = Product [Numeral 1, Numeral 2, Numeral 3]
  let i = Numeral 3
  let term = Apply (Apply f p) i
  let expectedResult = Numeral 3
  TestLabel
    "should evaluate to nth term of p for (Case n p)"
    $ assertEval term emptyEnv expectedResult

evalCase0 :: Test
evalCase0 = do
  let f = Lambda "p" (Base `Cross` Base `Cross` Base) $ Lambda "i" Base $ Case (Variable "i") (Variable "p")
  let p = Product [Numeral 1, Numeral 2, Numeral 3]
  let i = Numeral 0
  let term = Apply (Apply f p) i
  TestLabel
    "should evaluate to the emtpy product for (Case 0 p)"
    $ assertEval term emptyEnv emptyProduct

-- Using the strictness index of a function as the index of a case statement
evalCaseWithCatchAsNumeral :: Test
evalCaseWithCatchAsNumeral = do
  let f = Lambda "x" Base $ Lambda "y" Base $ Variable "x"
  let prod = Product $ map Numeral [0 .. 5]
  let strictIndex = Succ $ Catch f
  let term = Case strictIndex prod
  let expectedResult = Numeral 0
  TestLabel
    "should expect correct behavour when using Catch to generate a numeral for Case, including using hte correct index offset (0 or 1)"
    $ assertEval term emptyEnv expectedResult

assertEval :: Term -> Environment -> Term -> Test
assertEval term env expectedTerm =
  TestCase
    ( do
        result <- runEvalIO (eval term) env
        assertEqual
          ( "Term doesn't evaluate to the expected result. Term: "
              ++ show term
              ++ ". Environment: "
              ++ show env
          )
          expectedTerm
          result
    )