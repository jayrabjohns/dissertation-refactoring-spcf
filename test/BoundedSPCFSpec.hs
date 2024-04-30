module BoundedSPCFSpec where

import BoundedSPCF.AST
import BoundedSPCF.Evaluation
import Test.HUnit
import Utils.Environment

tests :: Test
tests =
  TestList
    [ evalCase,
      evalCase0,
      evalCaseWithCatchAsNumeral
    ]

evalCase :: Test
evalCase = do
  let f =
        Lambda "p" (Cross Nat 3) $
          Lambda "i" Nat $
            Case (Variable "i") (Variable "p")
  let p = Product [Numeral 1, Numeral 2, Numeral 3]
  let i = Numeral 2
  let term = Apply (Apply f p) i
  let expectedResult = Numeral 3
  TestLabel
    "should evaluate to nth term of p for (Case n p)"
    $ assertEval term empty expectedResult

evalCase0 :: Test
evalCase0 = do
  let f =
        Lambda "p" (Cross Nat 3) $
          Lambda "i" Nat $
            Case (Variable "i") (Variable "p")
  let p = Product [Numeral 1, Numeral 2, Numeral 3]
  let i = Numeral 0
  let term = Apply (Apply f p) i
  let expectedResult = Numeral 1
  TestLabel
    "should evaluate to the emtpy product for (Case 0 p)"
    $ assertEval term empty expectedResult

-- Using the strictness index of a function as the index of a case statement
evalCaseWithCatchAsNumeral :: Test
evalCaseWithCatchAsNumeral = do
  let f = Lambda "x" Nat $ Lambda "y" Nat $ Variable "x"
  let prod = Product $ map Numeral [0 .. 5]
  let strictIndex = Catch f
  let term = Case strictIndex prod
  let expectedResult = Numeral 0
  TestLabel
    "should expect correct behavour when using Catch to generate a numeral for Case, including using hte correct index offset (0 or 1)"
    $ assertEval term empty expectedResult

assertEval :: Term -> Environment Term -> Term -> Test
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