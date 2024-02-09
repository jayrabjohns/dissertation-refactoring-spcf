module BoundedSPCFSpec where

import BoundedSPCF
import qualified Data.Map as Map
import Test.HUnit

tests :: Test
tests =
  TestList
    [ trueIf0,
      falseIfNot0
    ]

evalVariable :: Test
evalVariable =
  TestLabel
    "should substitute a variable with a value from the environment"
    ( TestCase
        ( do
            let term = (Variable "x")
            let env = Map.fromList [("x", Nat 5)]
            let result = eval (Closure env term)
            case result of
              Right val -> assertEqual "should equal value in environment" (Nat 5) val
              Left err -> assertFailure err
        )
    )

trueIf0 :: Test
trueIf0 =
  TestLabel
    "should reduce an expression"
    ( TestCase
        ( do
            let term = (If0 (Variable "x") (Apply (Lambda "f" (Base :-> Base) (Variable "f")) (Variable ("y"))) (Variable "z"))
            let env = Map.fromList [("x", (Nat 0)), ("y", (Nat 3)), ("z", (Nat 6))]
            let result = eval (Closure env term)
            case result of
              Right val -> assertEqual "should equal 3" (Nat 3) val
              Left err -> assertFailure err
        )
    )

falseIfNot0 :: Test
falseIfNot0 =
  TestLabel
    "should reduce an expression"
    ( TestCase
        ( do
            let term = (If0 (Variable "x") (Apply (Lambda "f" (Base :-> Base) (Variable "f")) (Variable ("y"))) (Variable "z"))
            let env = Map.fromList [("x", (Nat 1)), ("y", (Nat 3)), ("z", (Nat 6))]
            let result = eval (Closure env term)
            case result of
              Right val -> assertEqual "should equal 6" (Nat 6) val
              Left err -> assertFailure err
        )
    )

assertSubstitution :: Term -> Environment -> Label -> Test
assertSubstitution term env expectedLabel =
  TestCase
    ( do
        let result = eval (Closure env term)
        let expectedVal = lookup
        case result of
          Right val -> assertEqual "should equal value in environment. " (Nat 5) val
          Left err -> assertFailure err
    )
